package bootstrap.liftweb

import javax.servlet.FilterConfig
import net.liftweb.http.LiftFilter


class VmforceLiftFilter extends LiftFilter {
  override def init(config: FilterConfig) {
    System.setProperty("run.mode", "production")
    super.init(config)
  }
}