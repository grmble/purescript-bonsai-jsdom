"use strict";

const jsdom = require('jsdom');

const jsdomOpts = {
  runScripts: 'dangerously',
  pretendToBeVisual: true,
};


exports.primitives =
  { jsdomWindow: function (str) {
      const dom = new jsdom.JSDOM(str, jsdomOpts);
      return dom.window;
    }

  , setProperty: function (name, value, elem) {
      elem[name] = value;
    }

  , fireClick: function (elem) {
      const win = elem.ownerDocument.defaultView;
      const ev = new win.MouseEvent('click', {
          view: elem.ownerDocument.defaultView,
          bubbles: true,
          cancelable: true
      });
      elem.dispatchEvent(ev);
    }

  , fireInput: function (elem) {
      const win = elem.ownerDocument.defaultView;
      const ev = new win.MouseEvent('input', {
          view: elem.ownerDocument.defaultView,
          bubbles: true,
          cancelable: true
      });
      elem.dispatchEvent(ev);
    }
  };
