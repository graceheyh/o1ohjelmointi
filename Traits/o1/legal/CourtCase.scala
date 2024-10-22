package o1.legal

// TODO: define class CourtCase properly
class CourtCase(val plaintiff: Entity, val defendant: Entity):
  override def toString: String = s"${plaintiff.name} v. ${defendant.name}"