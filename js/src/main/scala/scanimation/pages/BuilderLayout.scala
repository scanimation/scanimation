package scanimation.pages

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

  val contentId = BoxId()
  val sidebarId = BoxId()
  val sectionClass = BoxClass()
  val sectionTitleClass = BoxClass()
  val sectionContentClass = BoxClass()
  val framesOrZoneId = BoxId()
  val dropZoneId = BoxId()
  val frameListId = BoxId()
  val frameErrorClass = BoxClass()
  val frameRowClass = BoxClass()
  val frameControlClass = BoxClass()
  val frameSelectedClass = BoxClass()
  val frameWrapClass = BoxClass()
  val previewId = BoxId()
  val builderButtonClass = BoxClass()
  val framesControlsBoxId = BoxId()
  val settingsId = BoxId()
  val settingsWrapClass = BoxClass()
  val settingsInputClass = BoxClass()

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
      under(sectionClass).sub(
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
      ),
      isRegion && framesOrZoneId |> (
        _.borderWidth(1)
        ),
      under(dropZoneId).sub(
        isText |> (
          _.textSize(24)
          )
      ),
      under(frameListId).sub(
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
      ),
      isRegion && previewId |> (
        _.borderWidth(1),
        _.fillColor(greyHighlightColor)
      ),
      under(builderButtonClass).sub(
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
      ),
      isHBox && framesControlsBoxId |> (
        _.spacingX(8),
        ),
      under(settingsId).sub(
        isContainer && settingsWrapClass |> (
          _.fixedH(32)
          ),
        isRegion && settingsInputClass |> (
          _.fixedW(64),
          _.fixedH(22),
          _.fillColor(greyHighlightColor),
          _.borderWidth(1),
        ),
        isGrid |> (
          _.spacing(6 xy 0)
          )
      )
    )
  )

  /** Creates a section title with an icon */
  def section(textValue: String, iconValue: IconValue, content: Box): Box = {
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
  def builderButton(textValue: String, iconValue: IconValue): IconTextButtonBox = {
    iconTextButton(iconValue, textValue).addClass(builderButtonClass).fillX
  }

  /** Wraps the settings content into a fixed height box */
  def settingsWrap(content: Box): ContainerBox = {
    container.fillX.addClass(settingsWrapClass).sub(content.align(Vec2d.Left))
  }

  override def open(controller: Controller): Box = {
    val dropZone = container(dropZoneId).fillBoth.sub(
      text.as("Drop images here...")
    )
    val frameList = vbox(frameListId).fillX.align(Vec2d.Top).mutate { list =>
      controller.model.frames /> {
        case Nil =>
          list.hideDisplay
          dropZone.showDisplay
        case frames =>
          list.showDisplay
          dropZone.hideDisplay
          val previousList = list.layout.relChildren()
          val nextList = frames.zipWithIndex.map { case (frame, index) =>
            val iconValue = frame match {
              case Missing() =>
                MaterialDesign.HourglassEmpty
              case Loading(start) =>
                MaterialDesign.HourglassFull
              case Loaded(start, end, value) =>
                MaterialDesign.CheckCircle
              case Failed(start, end, reason) =>
                MaterialDesign.Error
            }
            val frameName = frame match {
              case Loaded(start, end, value) => value.name
              case _ => "N/A"
            }
            val controls = hbox.sub(
              button.addClass(frameControlClass).addClass(frameWrapClass)
                .sub(icon.as(MaterialDesign.ArrowUpward))
                .onClick(controller.moveFrameUp(index)),
              button.addClass(frameControlClass).addClass(frameWrapClass)
                .sub(icon.as(MaterialDesign.ArrowDownward))
                .onClick(controller.moveFrameDown(index)),
              button.addClass(frameControlClass).addClass(frameWrapClass)
                .sub(icon.as(MaterialDesign.Close))
                .onClick(controller.deleteFrame(index)),
            )
            val frameButton = button.fillX.sub(
              hbox.fillX.sub(
                container.addClass(frameWrapClass).sub(icon.as(iconValue).updateClass(frameErrorClass, frame.hasFailed)),
                text.as(frameName).fillX,
              )
            ).onClick(controller.selectFrame(index))
            val frameRowContent = hbox.fillX
            region.addClass(frameRowClass).fillX.sub(frameRowContent).mutate { row =>
              controller.model.selectedFrame /> {
                case Some(i) if i == index =>
                  row.addClass(frameSelectedClass)
                  frameButton.addClass(frameSelectedClass)
                  frameRowContent.sub(frameButton, controls)
                case _ =>
                  row.removeClass(frameSelectedClass)
                  frameButton.removeClass(frameSelectedClass)
                  frameRowContent.sub(frameButton).calculateLayoutX()
              }
            }
          }
          list.subs(nextList)
          previousList.foreach(b => context.unregister(b))
      }
    }
    val framesOrZone = region(framesOrZoneId).fillBoth.sub(
      dropZone,
      frameList
    )
    region(builderId).fillBoth.sub(
      hbox(contentId).fillBoth.sub(
        vbox(sidebarId).fillY.sub(
          section(
            "Animation Frames", MaterialDesign.PhotoLibrary,
            vbox.fillBoth.sub(
              framesOrZone,
              hbox(framesControlsBoxId).fillX.sub(
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
          ).fillY,
          section(
            "Settings", MaterialDesign.SettingsApplications,
            vbox.fillX.sub(
              vbox(settingsId).fillX.sub(
                settingsWrap(text.mutate { text =>
                  controller.model.frames /> {
                    case frames =>
                      val label = frames
                        .collectFirst { case Loaded(start, end, frame) => frame.size }
                        .map { size => s"Image size: ${size.x} x ${size.y}" }
                        .getOrElse("Image size: N/A")
                      text.as(label)
                  }
                }),
                settingsWrap(text.mutate { text =>
                  controller.model.frames /> {
                    case frames => text.as(s"Frame count: ${frames.size} frames")
                  }
                }),
                grid().fillX
                  .mutate(_.columns(3))
                  .sub(
                    settingsWrap(text.as("Frame width:")),
                    settingsWrap(region.addClass(settingsInputClass)).fillX(0),
                    settingsWrap(text.as("%")),

                    settingsWrap(text.as("Frame width:")),
                    settingsWrap(region.addClass(settingsInputClass)).fillX(0),
                    settingsWrap(text.as("px")),

                    settingsWrap(text.as("Frame overlap:")),
                    settingsWrap(region.addClass(settingsInputClass)).fillX(0),
                    settingsWrap(text.as("frames")),
                  )
              ),
              builderButton("Reset to defaults", MaterialDesign.SettingsBackupRestore)
                .onClick(controller.resetSettings())
            )
          ),
          section(
            "Process and export", MaterialDesign.Cloud,
            vbox.fillX.sub(
              builderButton("", MaterialDesign.FlashOn)
                .onClick(controller.computeScanimation())
                .mutate { button =>
                  (controller.model.frames && controller.model.scanimation) /> {
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
        ),
        section(
          "Preview", MaterialDesign.Theaters,
          region(previewId).fillY
        ).fillY
      )
    )
  }

  implicit class BuilderButtonOps(val button: IconTextButtonBox) extends AnyVal {
    /** Makes button interactive only when scanimation is produced */
    def enableOnFrames(controller: Controller): IconTextButtonBox = {
      controller.model.frames /> {
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