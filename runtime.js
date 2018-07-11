const entities = new (require('html-entities').XmlEntities)();

function htmlEscapeEntities(val) {
  return entities.encode(val);
}

