package scanimation.pages

import scanimation.box.IconStyle.IconValue
import scanimation.box._
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
  val settingsId = BoxId()
  val sectionClass = BoxClass()
  val sectionTitleClass = BoxClass()
  val framesOrZoneId = BoxId()
  val dropZoneId = BoxId()
  val previewId = BoxId()

  val testRegionClass = BoxClass()

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
      isRegion && testRegionClass |> (
        _.fillColor(highlightColor)
        ),
      isVBox && settingsId |> (
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
        )
      ),
      isRegion && framesOrZoneId |> (
        _.borderWidth(1)
        ),
      under(dropZoneId).sub(
        isText |> (
          _.textSize(24)
          )
      ),
      isRegion && previewId |> (
        _.borderWidth(1),
        _.fillColor(highlightColor)
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
      content.fillX
    )
  }

  override def open(controller: Controller): Box = {
    val dropZone = container(dropZoneId).fillBoth.sub(
      text.as("Drop images here...")
    )
    val frameList = container.fillBoth.mutate(_.layout.relVisible.write(false))
    val framesOrZone = region(framesOrZoneId).fillBoth.sub(
      dropZone,
      frameList
    )
    region(builderId).fillBoth.sub(
      hbox(contentId).fillBoth.sub(
        vbox(settingsId).fillY.sub(
          section(
            "Animation Frames", MaterialDesign.PhotoLibrary,
            framesOrZone
          ).fillY,
          section(
            "Settings", MaterialDesign.SettingsApplications,
            region.fixedH(200)
          ),
          section(
            "Process and export", MaterialDesign.Cloud,
            region.fixedH(200)
          )
        ),
        section(
          "Preview", MaterialDesign.Theaters,
          region(previewId).fillY
        ).fillY
      )
    )
  }
}