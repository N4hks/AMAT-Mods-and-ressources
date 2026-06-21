/// 14mm CCW thread Suppressor for a 9mm bore gun belongs to supressors ///
class Attachment14mmCCW_9mmSuppressorClass {}

Attachment14mmCCW_9mmSuppressorClass Attachment14mmCCW_9mmSuppressorSource;
class Attachment14mmCCW_9mmSuppressor : AttachmentSuppressor {}

/// 14mm CCW thread on a 9mm bore gun  belongs to 14mm CCW thread Suppressor for a 9mm bore gun ///
class Attachment14mmCCW_9mmClass {}

Attachment14mmCCW_9mmClass Attachment14mmCCW_9mmSource;
class Attachment14mmCCW_9mm : Attachment14mmCCW_9mmSuppressor {}

/// 14mm CCW thread belongs to 14mm CCW thread on a 9mm bore gun ///
class Attachment14mmCCWClass {}

Attachment14mmCCWClass Attachment14mmCCWSource;
class Attachment14mmCCW : Attachment14mmCCW_9mm{}
