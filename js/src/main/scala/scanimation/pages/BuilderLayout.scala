package scanimation.pages

import lib.dropzone._
import scanimation.box.BoxClass._
import scanimation.box.IconStyle.IconValue
import scanimation.box._
import scanimation.common.Transition._
import scanimation.common._
import scanimation.icon.MaterialDesign
import scanimation.mvc._
import scanimation.pages.pages.JqBoxLayout
import scanimation.style._
import scanimation.util.logging.Logging

/** Dragons page layout */
object BuilderLayout extends JqBoxLayout[BuilderPage] with Logging {
  override protected def logKey: String = "builder"

  /** Id of the builder page content */
  val contentId = BoxId()
  /** Id of the settings sidebar on the page */
  val sidebarId = BoxId()
  /** Class describing the section with title and content */
  val sectionClass = BoxClass()
  /** Class describing the section title with icon and text */
  val sectionTitleClass = BoxClass()
  /** Class describing the section content */
  val sectionContentClass = BoxClass()
  /** Id of the box that contains either frame list or a drop zone */
  val framesOrZoneId = BoxId()
  /** Id of the box where user can drag the frames */
  val dropZoneId = BoxId()

  /** Id of the box displaying all frames */
  val frameListId = BoxId()
  /** Class describing frames that failed to load */
  val frameErrorClass = BoxClass()
  /** Class describing a single frame row */
  val frameRowClass = BoxClass()
  /** Class describing buttons that manipulate the frame */
  val frameControlClass = BoxClass()
  /** Class assigned to frame rows that are selected */
  val frameSelectedClass = BoxClass()
  /** Class that wraps the frame elements into certain size */
  val frameWrapClass = BoxClass()
  /** Id of the box containing frame list controls */
  val frameListControlsBoxId = BoxId()

  /** Id of the box that previews the images */
  val previewId = BoxId()
  /** Class for all builder buttons */
  val builderButtonClass = BoxClass()
  /** Id of the section content with settings */
  val settingsId = BoxId()
  /** Class used to resize all of the settings rows */
  val settingsWrapClass = BoxClass()
  /** Class describing input fields for settings */
  val settingsInputClass = BoxClass()

  /** Style for each builder section */
  private val sectionStyle = under(sectionClass).sub(
    isVBox |> (
      _.spacingY(8)
      ),
    under(sectionTitleClass).sub(
      isHBox |> (
        _.spacingX(8),
        _.fixedH(40),
      ),
      isIcon |> (
        _.iconSize(32),
        _.iconColor(primaryColor),
      ),
      isText |> (
        _.textSize(24),
        )
    ),
    isVBox && sectionContentClass |> {
      _.spacingY(8)
    }
  )

  /** Style for dropzone box */
  private val dropZoneStyle = under(dropZoneId).sub(
    isText |> (
      _.textSize(24)
      )
  )

  /** Style for frame list box */
  private val frameListStyle = under(frameListId).sub(
    isVBox |> (
      _.spacingY(0)
      ),
    under(frameRowClass).sub(
      isButton |> (
        _.fixedH(32),
        _.cursor(Cursors.Auto)
      ),
      isButton && frameSelectedClass |> (
        _.fillColor(greyHighlightColor)
        ),
      isButton && Hover |> (
        _.fillColor(greyHighlightColor),
        _.cursor(Cursors.Pointer)
      ),
      isRegion && frameSelectedClass |> (
        _.fillColor(greyHighlightColor)
        ),
      frameWrapClass |> (
        _.fixedW(32)
        ),
      isIcon |> (
        _.iconSize(16),
        _.iconColor(blackColor),
      ),
      isIcon && frameErrorClass |> (
        _.iconColor(errorColor)
        ),
      isText |> (
        _.textSize(16),
        ),
      isButton && frameControlClass |> (
        _.fillColor(greyHighlightColor)
        ),
      isButton && frameControlClass && Hover |> (
        _.fillColor(primaryHighlightColor)
        )
    )
  )

  /** Style for frame list controls buttons */
  private val frameListControlsStyle = isHBox && frameListControlsBoxId |> (
    _.spacingX(8),
    )

  /** Style for preview content */
  private val previewStyle = isRegion && previewId |> (
    _.borderWidth(1),
    _.fillColor(greyHighlightColor)
  )

  /** Style for settings section */
  private val settingsStyle = under(settingsId).sub(
    isContainer && settingsWrapClass |> (
      _.fixedH(32)
      ),
    inInput && settingsInputClass |> (
      _.fixedW(64),
      _.fixedH(22),
      _.fillColor(whiteColor),
      _.borderWidth(1),
      _.borderColor(blackColor),
      _.textSize(16),
      _.textFont(robotoSlab),
    ),
    inInput && settingsInputClass && Invalid |> (
      _.borderColor(errorColor),
      ),
    isGrid |> (
      _.spacing(6 xy 0)
      )
  )

  /** Common style for builder page buttons */
  private val builderButtonStyle = under(builderButtonClass).sub(
    isButton |> (
      _.fillColor(primaryColor),
      _.fixedH(40),
      _.cursor(Cursors.Auto)
    ),
    isButton && Hover |> (
      _.fillColor(primaryColor.lighter),
      _.cursor(Cursors.Pointer),
    ),
    isButton && Disabled |> (
      _.fillColor(disabledColor),
      _.cursor(Cursors.Auto)
    ),
    isHBox |> (
      _.spacingX(4)
      ),
    isIcon |> (
      _.iconColor(whiteColor),
      _.iconSize(20),
    ),
    isIcon && Disabled |> (
      _.iconColor(greyHighlightColor),
      ),
    isText |> (
      _.textColor(whiteColor),
      _.textSize(16),
    ),
    isText && Disabled |> (
      _.textColor(greyHighlightColor),
      ),
  )

  /** Complete stylesheet */
  private implicit val stylesheet: Styler = StyleSheet(
    under(builderId).sub(
      isRegion |> (
        _.fillColor(whiteColor),
        ),
      isText |> (
        _.textFont(robotoSlab)
        ),
      isHBox && contentId |> (
        _.pad(32 xy 32),
        _.spacingX(32),
      ),
      isVBox && sidebarId |> (
        _.fixedW(300),
        _.spacingY(16),
      ),
      isRegion && framesOrZoneId |> (
        _.borderWidth(1)
        ),
      sectionStyle,
      dropZoneStyle,
      frameListStyle,
      frameListControlsStyle,
      previewStyle,
      builderButtonStyle,
      settingsStyle,
    )
  )

  /** Returns the parent box for the page layout when page opens */
  override def open(controller: Controller): Box = {
    region(builderId).fillBoth.sub(
      hbox(contentId).fillBoth.sub(
        vbox(sidebarId).fillY.sub(
          buildFramesSection(controller).fillY,
          buildSettingsSection(controller),
          buildProcessSection(controller)
        ),
        buildPreviewSection(controller).fillY
      )
    )
  }

  /** Creates a section title with an icon */
  private def section(textValue: String, iconValue: IconValue, content: Box): Box = {
    vbox.addClass(sectionClass).fillX.sub(
      hbox.addClass(sectionTitleClass).fillX.sub(
        icon.as(iconValue),
        text.as(textValue),
        container.fillBoth,
      ),
      content.addClass(sectionContentClass).fillX
    )
  }

  /** Creates a button with an icon for builder page */
  private def builderButton(textValue: String, iconValue: IconValue): IconTextButtonBox = {
    iconTextButton(iconValue, textValue).addClass(builderButtonClass).fillX
  }

  /** Wraps the settings content into a fixed height box */
  private def settingsWrap(content: Box): ContainerBox = {
    container.fillX.addClass(settingsWrapClass).sub(content.align(Vec2d.Left))
  }

  /** Returns the builder section for frames */
  private def buildFramesSection(controller: Controller): Box = {
    val dropZone = buildDropZone(controller)
    val frameList = buildFrameList(controller)
    val framesOrZone = region(framesOrZoneId).fillBoth.mutate { parent =>
      controller.model.frames.data /> {
        case Nil => parent.sub(dropZone)
        case _ => parent.sub(frameList).calculateLayoutX()
      }
    }
    section(
      "Animation Frames", MaterialDesign.PhotoLibrary,
      vbox.fillBoth.sub(
        framesOrZone,
        hbox(frameListControlsBoxId).fillX.sub(
          builderButton("Add", MaterialDesign.AddCircle)
            .onClick(controller.addFrames()),
          builderButton("Clear", MaterialDesign.Cancel)
            .onClick(controller.clearFrames())
            .enableOnFrames(controller),
        ),
        builderButton("Show animation", MaterialDesign.PlayArrow)
          .onClick(controller.showAnimation())
          .enableOnFrames(controller),
      )
    )
  }

  /** Returns dropzone box */
  private def buildDropZone(controller: Controller): Box = {
    container(dropZoneId)
      .fillBoth
      .dropzone("/foo")
      .sub(
        text.as("Drop images here...")
      )
  }

  /** Returns the frame list box */
  private def buildFrameList(controller: Controller): Box = {
    vbox(frameListId).fillX.align(Vec2d.Top).bindList[TransitionData[Frame]](
      data = controller.model.frames,
      createCode = { view =>
        val frame = view.read()
        val frameIcon = icon.mutate { icon =>
          frame /> {
            case Missing() =>
              icon.as(MaterialDesign.HourglassEmpty)
            case Loading(start) =>
              icon.as(MaterialDesign.HourglassFull)
            case Loaded(start, end, value) =>
              icon.as(MaterialDesign.CheckCircle)
            case Failed(start, end, reason) =>
              icon.as(MaterialDesign.Error)
          }
          frame /> {
            case failure if failure.hasFailed =>
              icon.addClass(frameErrorClass)
            case _ =>
              icon.removeClass(frameErrorClass)
          }
        }
        val frameName = text.fillX.mutate { text =>
          frame /> {
            case Loaded(start, end, value) =>
              text.as(value.name)
            case _ =>
              text.as("N/A")
          }
        }
        val controls = hbox.sub(
          button.addClass(frameControlClass).addClass(frameWrapClass)
            .sub(icon.as(MaterialDesign.ArrowUpward))
            .onClick(view.moveUp()),
          button.addClass(frameControlClass).addClass(frameWrapClass)
            .sub(icon.as(MaterialDesign.ArrowDownward))
            .onClick(view.moveDown()),
          button.addClass(frameControlClass).addClass(frameWrapClass)
            .sub(icon.as(MaterialDesign.Close))
            .onClick(view.remove()),
        )
        val frameIconText = button.fillX.sub(
          hbox.fillX.sub(
            container.addClass(frameWrapClass).sub(frameIcon),
            frameName,
          )
        ).onClick(view.select())
        val frameRowContent = hbox.fillX
        region.addClass(frameRowClass).fillX.sub(frameRowContent).mutate { row =>
          row.layout.classes /> {
            case selected if selected.contains(frameSelectedClass) =>
              frameIconText.addClass(frameSelectedClass)
              frameRowContent.sub(frameIconText, controls)
            case _ =>
              frameIconText.removeClass(frameSelectedClass)
              frameRowContent.sub(frameIconText)
          }
        }
      },
      selectCode = {
        case (row, selected) =>
          row.updateClass(frameSelectedClass, selected)
      }
    )
  }

  /** Returns the builder settings section */
  private def buildSettingsSection(controller: Controller): Box = {
    section(
      "Settings", MaterialDesign.SettingsApplications,
      vbox.fillX.sub(
        vbox(settingsId).fillX.sub(
          settingsWrap(text.mutate { text =>
            controller.model.frameSize /> {
              case Some(size) => text.as(s"Image size: ${size.x} x ${size.y}")
              case None => text.as(s"Image size: N/A")
            }
          }),
          settingsWrap(text.mutate { text =>
            controller.model.frameCount /> {
              case Some(count) => text.as(s"Frame count: $count frames")
              case None => text.as("Frame count: N/A")
            }
          }),
          grid().fillX
            .mutate(_.columns(3))
            .sub(
              settingsWrap(text.as("Frame width:")),
              settingsWrap(
                input
                  .validation("[0-9]+")
                  .addClass(settingsInputClass)
                  .mutate { input =>
                    controller.model.frameWidth /> { case Some(value) => input.as(value.toString) }
                  }
                  .onChange { case value => controller.setFrameWidth(value.map(s => s.toInt)) }
              ).fillX(0),
              settingsWrap(text.as("px")),

              settingsWrap(text.as("Frame overlap:")),
              settingsWrap(
                input
                  .validation("[0-9]+")
                  .addClass(settingsInputClass)
                  .mutate { input =>
                    controller.model.frameOverlap /> { case Some(value) => input.as(value.toString) }
                  }
                  .onChange { case value => controller.setFrameOverlap(value.map(s => s.toInt)) }
              ).fillX(0),
              settingsWrap(text.as("frames")),
            )
        ),
        builderButton("Reset to defaults", MaterialDesign.SettingsBackupRestore)
          .onClick(controller.resetSettings())
      )
    )
  }

  /** Returns the builder section for processing */
  private def buildProcessSection(controller: Controller): Box = {
    section(
      "Process and export", MaterialDesign.Cloud,
      vbox.fillX.sub(
        builderButton("", MaterialDesign.FlashOn)
          .onClick(controller.computeScanimation())
          .mutate { button =>
            (controller.model.frames.data && controller.model.scanimation) /> {
              case (emptyOrOne, _) if emptyOrOne.size <= 1 =>
                button.disable
                button.as("Compute scanimation")
                button.icon.as(MaterialDesign.FlashOn)
              case (_, Missing()) =>
                button.enable
                button.as("Compute scanimation")
                button.icon.as(MaterialDesign.FlashOn)
              case (_, Loading(start)) =>
                button.disable
                button.as("Processing...")
                button.icon.as(MaterialDesign.Timer)
              case (_, Loaded(start, end, value)) =>
                button.disable
                button.as("Done!")
                button.icon.as(MaterialDesign.CheckCircle)
              case _ => // ignore
            }
          },
        builderButton("Show scanimation", MaterialDesign.PlayCircleFilled)
          .onClick(controller.showScanimation())
          .enableOnScanimation(controller),
        builderButton("Download scanimation", MaterialDesign.Image)
          .onClick(controller.exportScanimation())
          .enableOnScanimation(controller),
        builderButton("Download grid", MaterialDesign.Texture)
          .onClick(controller.exportGrid())
          .enableOnScanimation(controller),
      )
    )
  }

  /** Returns a builder section that previews images */
  private def buildPreviewSection(controller: Controller): Box = {
    section(
      "Preview", MaterialDesign.Theaters,
      region(previewId).fillY
    )
  }

  implicit class BuilderButtonOps(val button: IconTextButtonBox) extends AnyVal {
    /** Makes button interactive only when scanimation is produced */
    def enableOnFrames(controller: Controller): IconTextButtonBox = {
      controller.model.frames.data /> {
        case Nil => button.disable
        case _ => button.enable
      }
      button
    }

    /** Makes button interactive only when scanimation is produced */
    def enableOnScanimation(controller: Controller): IconTextButtonBox = {
      controller.model.scanimation /> {
        case Loaded(start, end, value) => button.enable
        case _ => button.disable
      }
      button
    }
  }

}