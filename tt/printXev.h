/*static void
hexdump_event(const xcb_generic_event_t *ev, FILE *out)
{
  return;
  const uint32_t *words = (const uint32_t *)ev;
  int nBytes = words[1] * 4;
  const uint8_t *p = (const uint8_t *)ev;
  fprintf(out, "  raw (%d bytes):", nBytes);
  for (int i = 0; i < nBytes; ++i) {
    if (i % 8 == 0) fprintf(out, "\n    ");
    fprintf(out, "%02x ", p[i]);
  }
  fprintf(out, "\n");
}*/

void
printXev(const xcb_generic_event_t *ev, FILE *out) {
  if (!ev || !out) return;
  uint8_t evTyp = ev->response_type & 0x7f;
  switch (evTyp) {
  case XCB_KEY_PRESS: case XCB_KEY_RELEASE:
    const xcb_key_press_event_t *e = (const xcb_key_press_event_t *)ev;
    const char *name = (evTyp == XCB_KEY_PRESS) ? "KeyPress" : "KeyRelease";
    fprintf(out, "%s:\n"
      "  time        = %u\n"
      "  root        = 0x%08x\n"
      "  event       = 0x%08x\n"
      "  child       = 0x%08x\n"
      "  root_x      = %d\n"
      "  root_y      = %d\n"
      "  event_x     = %d\n"
      "  event_y     = %d\n"
      "  state       = 0x%04x\n"
      "  keycode     = %u\n"
      "  same_screen = %u\n",
      name,
      e->time,
      e->root, e->event, e->child,
      e->root_x, e->root_y,
      e->event_x, e->event_y,
      e->state,
      e->detail,
      e->same_screen);
    break;
  case XCB_BUTTON_PRESS: case XCB_BUTTON_RELEASE: {
    const xcb_button_press_event_t *e = (const xcb_button_press_event_t *)ev;
    const char *name = (evTyp == XCB_BUTTON_PRESS) ? "ButtonPress" : "ButtonRelease";
    fprintf(out,
      "%s:\n"
      "  time        = %u\n"
      "  root        = 0x%08x\n"
      "  event       = 0x%08x\n"
      "  child       = 0x%08x\n"
      "  root_x      = %d\n"
      "  root_y      = %d\n"
      "  event_x     = %d\n"
      "  event_y     = %d\n"
      "  state       = 0x%04x\n"
      "  button      = %u\n"
      "  same_screen = %u\n",
      name,
      e->time,
      e->root, e->event, e->child,
      e->root_x, e->root_y,
      e->event_x, e->event_y,
      e->state,
      e->detail,
      e->same_screen);
    break;}
  case XCB_MOTION_NOTIFY: {
    const xcb_motion_notify_event_t *e = (const xcb_motion_notify_event_t *)ev;
    fprintf(out,
            "motionnotify:\n"
            "  time        = %u\n"
            "  root        = 0x%08x\n"
            "  event       = 0x%08x\n"
            "  child       = 0x%08x\n"
            "  root_x      = %d\n"
            "  root_y      = %d\n"
            "  event_x     = %d\n"
            "  event_y     = %d\n"
            "  state       = 0x%04x\n"
            "  same_screen = %u\n",
            e->time,
            e->root, e->event, e->child,
            e->root_x, e->root_y,
            e->event_x, e->event_y,
            e->state,
            e->same_screen);
    break;
    }
  case XCB_ENTER_NOTIFY: case XCB_LEAVE_NOTIFY: {
    const xcb_enter_notify_event_t *e = (const xcb_enter_notify_event_t *)ev;
    const char *name = (evTyp == XCB_ENTER_NOTIFY) ? "enternotify" : "leavenotify";
    const char *detail_str;
    switch (e->detail) {
    case XCB_NOTIFY_DETAIL_ANCESTOR:          detail_str = "ancestor";         break;
    case XCB_NOTIFY_DETAIL_VIRTUAL:           detail_str = "virtual";          break;
    case XCB_NOTIFY_DETAIL_INFERIOR:          detail_str = "inferior";         break;
    case XCB_NOTIFY_DETAIL_NONLINEAR:         detail_str = "nonlinear";        break;
    case XCB_NOTIFY_DETAIL_NONLINEAR_VIRTUAL: detail_str = "nonlinearvirtual"; break;
    case XCB_NOTIFY_DETAIL_POINTER:           detail_str = "pointer";          break;
    case XCB_NOTIFY_DETAIL_POINTER_ROOT:      detail_str = "pointerroot";      break;
    default:                                  detail_str = "unknown";
    }
    const char *mode_str = (e->mode == XCB_NOTIFY_MODE_NORMAL)   ? "normal"
                         : (e->mode == XCB_NOTIFY_MODE_GRAB)    ? "grab"
                         : (e->mode == XCB_NOTIFY_MODE_UNGRAB)  ? "ungrab"
                         : (e->mode == XCB_NOTIFY_MODE_WHILE_GRABBED) ? "whilegrabbed"
                         : "unknown";
    fprintf(out,
            "%s:\n"
            "  detail      = %s (%u)\n"
            "  mode        = %s (%u)\n"
            "  time        = %u\n"
            "  root        = 0x%08x\n"
            "  event       = 0x%08x\n"
            "  child       = 0x%08x\n"
            "  root_x      = %d\n"
            "  root_y      = %d\n"
            "  event_x     = %d\n"
            "  event_y     = %d\n"
            "  state       = 0x%04x\n"
            "  same_screen_focus = %u\n",
            name,
            detail_str, e->detail,
            mode_str, e->mode,
            e->time,
            e->root, e->event, e->child,
            e->root_x, e->root_y,
            e->event_x, e->event_y,
            e->state,
            e->same_screen_focus);
    break;
    }
  case XCB_FOCUS_IN: case XCB_FOCUS_OUT: {
  const xcb_focus_in_event_t *e = (const xcb_focus_in_event_t *)ev;
  const char *name = (evTyp == XCB_FOCUS_IN) ? "FocusIn" : "FocusOut";
  const char *detail_str = (e->detail == XCB_NOTIFY_DETAIL_ANCESTOR) ? "Ancestor"
                        : (e->detail == XCB_NOTIFY_DETAIL_POINTER)  ? "Pointer"
                        : (e->detail == XCB_NOTIFY_DETAIL_POINTER_ROOT) ? "PointerRoot"
                        : (e->detail == XCB_NOTIFY_DETAIL_NONE)    ? "None"
                        : "unknown";
  const char *mode_str = (e->mode == XCB_NOTIFY_MODE_NORMAL) ? "Normal"
                       : (e->mode == XCB_NOTIFY_MODE_GRAB)   ? "Grab"
                       : (e->mode == XCB_NOTIFY_MODE_UNGRAB) ? "Ungrab"
                       : (e->mode == XCB_NOTIFY_MODE_WHILE_GRABBED) ? "WhileGrabbed"
                       : "Unknown";
  fprintf(out,
          "%s:\n"
          "  detail = %s (%u)\n"
          "  mode   = %s (%u)\n"
          "  window = 0x%08x\n",
          name,
          detail_str, e->detail,
          mode_str, e->mode,
          e->event);
  break;
  }

  case XCB_KEYMAP_NOTIFY: {
  const xcb_keymap_notify_event_t *e = (const xcb_keymap_notify_event_t *)ev;
  fprintf(out, "KeymapNotify:\n");
  for (int i = 0; i < 31; ++i) {
      if (i % 8 == 0) fprintf(out, "  ");
      fprintf(out, "%02x ", e->keys[i]);
      if (i % 8 == 7) fprintf(out, "\n");
  }
  if (31 % 8) fprintf(out, "\n");
  break;
  }

  case XCB_EXPOSE: {
  const xcb_expose_event_t *e = (const xcb_expose_event_t *)ev;
  fprintf(out,
          "Expose:\n"
          "  window = 0x%08x\n"
          "  x      = %u\n"
          "  y      = %u\n"
          "  width  = %u\n"
          "  height = %u\n"
          "  count  = %u\n",
          e->window, e->x, e->y, e->width, e->height, e->count);
  break;
  }

  case XCB_VISIBILITY_NOTIFY: {
  const xcb_visibility_notify_event_t *e = (const xcb_visibility_notify_event_t *)ev;
  const char *state_str = (e->state == XCB_VISIBILITY_UNOBSCURED) ? "Unobscured"
                        : (e->state == XCB_VISIBILITY_PARTIALLY_OBSCURED) ? "PartiallyObscured"
                        : (e->state == XCB_VISIBILITY_FULLY_OBSCURED) ? "FullyObscured"
                        : "Unknown";
  fprintf(out,
          "VisibilityNotify:\n"
          "  window = 0x%08x\n"
          "  state  = %s (%u)\n",
          e->window, state_str, e->state);
  break;
  }

  case XCB_CREATE_NOTIFY: {
  const xcb_create_notify_event_t *e = (const xcb_create_notify_event_t *)ev;
  fprintf(out,
          "CreateNotify:\n"
          "  parent   = 0x%08x\n"
          "  window   = 0x%08x\n"
          "  x        = %d\n"
          "  y        = %d\n"
          "  width    = %u\n"
          "  height   = %u\n"
          "  border   = %u\n"
          "  override = %u\n",
          e->parent, e->window,
          e->x, e->y,
          e->width, e->height,
          e->border_width,
          e->override_redirect);
  break;
  }

  case XCB_DESTROY_NOTIFY: {
  const xcb_destroy_notify_event_t *e = (const xcb_destroy_notify_event_t *)ev;
  fprintf(out,
          "DestroyNotify:\n"
          "  event  = 0x%08x\n"
          "  window = 0x%08x\n",
          e->event, e->window);
  break;
  }

  case XCB_UNMAP_NOTIFY: {
  const xcb_unmap_notify_event_t *e = (const xcb_unmap_notify_event_t *)ev;
  fprintf(out,
          "UnmapNotify:\n"
          "  event          = 0x%08x\n"
          "  window         = 0x%08x\n"
          "  from_configure = %u\n",
          e->event, e->window, e->from_configure);
  break;
  }

  case XCB_MAP_NOTIFY: {
  const xcb_map_notify_event_t *e = (const xcb_map_notify_event_t *)ev;
  fprintf(out,
          "MapNotify:\n"
          "  event    = 0x%08x\n"
          "  window   = 0x%08x\n"
          "  override = %u\n",
          e->event, e->window, e->override_redirect);
  break;
  }

  case XCB_MAP_REQUEST: {
  const xcb_map_request_event_t *e = (const xcb_map_request_event_t *)ev;
  fprintf(out,
          "MapRequest:\n"
          "  parent = 0x%08x\n"
          "  window = 0x%08x\n",
          e->parent, e->window);
  break;
  }

  case XCB_REPARENT_NOTIFY: {
  const xcb_reparent_notify_event_t *e = (const xcb_reparent_notify_event_t *)ev;
  fprintf(out,
          "ReparentNotify:\n"
          "  event    = 0x%08x\n"
          "  window   = 0x%08x\n"
          "  parent   = 0x%08x\n"
          "  x        = %d\n"
          "  y        = %d\n"
          "  override = %u\n",
          e->event, e->window, e->parent,
          e->x, e->y,
          e->override_redirect);
  break;
  }

  case XCB_CONFIGURE_NOTIFY: {
  const xcb_configure_notify_event_t *e = (const xcb_configure_notify_event_t *)ev;
  fprintf(out,
          "ConfigureNotify:\n"
          "  event         = 0x%08x\n"
          "  window        = 0x%08x\n"
          "  above_sibling = 0x%08x\n"
          "  x,y           = %d,%d\n"
          "  width,height  = %u,%u\n"
          "  border_width  = %u\n"
          "  override      = %u\n",
          e->event, e->window, e->above_sibling,
          e->x, e->y,
          e->width, e->height,
          e->border_width,
          e->override_redirect);
  break;
  }

  case XCB_CONFIGURE_REQUEST: {
  const xcb_configure_request_event_t *e = (const xcb_configure_request_event_t *)ev;
  fprintf(out,
          "ConfigureRequest:\n"
          "  parent       = 0x%08x\n"
          "  window       = 0x%08x\n"
          "  sibling      = 0x%08x\n"
          "  stack_mode   = %u\n"
          "  x,y          = %d,%d\n"
          "  width,height = %u,%u\n"
          "  border_width = %u\n"
          "  value_mask   = 0x%04x\n",
          e->parent, e->window, e->sibling,
          e->stack_mode,
          e->x, e->y,
          e->width, e->height,
          e->border_width,
          e->value_mask);
  break;
  }

  case XCB_GRAVITY_NOTIFY: {
  const xcb_gravity_notify_event_t *e = (const xcb_gravity_notify_event_t *)ev;
  fprintf(out,
          "GravityNotify:\n"
          "  event  = 0x%08x\n"
          "  window = 0x%08x\n"
          "  x,y    = %d,%d\n",
          e->event, e->window, e->x, e->y);
  break;
  }

  case XCB_RESIZE_REQUEST: {
  const xcb_resize_request_event_t *e = (const xcb_resize_request_event_t *)ev;
  fprintf(out,
          "ResizeRequest:\n"
          "  window = 0x%08x\n"
          "  width  = %u\n"
          "  height = %u\n",
          e->window, e->width, e->height);
  break;
  }

  case XCB_CIRCULATE_NOTIFY:
  case XCB_CIRCULATE_REQUEST: {
  const xcb_circulate_notify_event_t *e = (const xcb_circulate_notify_event_t *)ev;
  const char *name = (evTyp == XCB_CIRCULATE_NOTIFY) ? "CirculateNotify" : "CirculateRequest";
  const char *place = (e->place == XCB_PLACE_ON_TOP)    ? "OnTop"
                   : (e->place == XCB_PLACE_ON_BOTTOM) ? "OnBottom"
                   : "Unknown";
  fprintf(out,
          "%s:\n"
          "  event  = 0x%08x\n"
          "  window = 0x%08x\n"
          "  place  = %s (%u)\n",
          name,
          e->event, e->window,
          place, e->place);
  break;
  }

  case XCB_PROPERTY_NOTIFY: {
  const xcb_property_notify_event_t *e = (const xcb_property_notify_event_t *)ev;
  const char *state = (e->state == XCB_PROPERTY_NEW_VALUE) ? "NewValue"
                     : (e->state == XCB_PROPERTY_DELETE)   ? "Deleted"
                     : "Unknown";
  fprintf(out,
          "PropertyNotify:\n"
          "  window = 0x%08x\n"
          "  atom   = %u\n"
          "  time   = %u\n"
          "  state  = %s (%u)\n",
          e->window, e->atom, e->time, state, e->state);
  break;
  }

  case XCB_SELECTION_CLEAR: {
  const xcb_selection_clear_event_t *e = (const xcb_selection_clear_event_t *)ev;
  fprintf(out,
          "SelectionClear:\n"
          "  time      = %u\n"
          "  owner     = 0x%08x\n"
          "  selection = %u\n",
          e->time, e->owner, e->selection);
  break;
  }

  case XCB_SELECTION_REQUEST: {
  const xcb_selection_request_event_t *e = (const xcb_selection_request_event_t *)ev;
  fprintf(out,
          "SelectionRequest:\n"
          "  time      = %u\n"
          "  owner     = 0x%08x\n"
          "  requestor = 0x%08x\n"
          "  selection = %u\n"
          "  target    = %u\n"
          "  property  = %u\n",
          e->time, e->owner, e->requestor,
          e->selection, e->target, e->property);
  break;
  }

  case XCB_SELECTION_NOTIFY: {
  const xcb_selection_notify_event_t *e = (const xcb_selection_notify_event_t *)ev;
  fprintf(out,
          "SelectionNotify:\n"
          "  time      = %u\n"
          "  requestor = 0x%08x\n"
          "  selection = %u\n"
          "  target    = %u\n"
          "  property  = %u\n",
          e->time, e->requestor,
          e->selection, e->target, e->property);
  break;
  }

  case XCB_COLORMAP_NOTIFY: {
  const xcb_colormap_notify_event_t *e = (const xcb_colormap_notify_event_t *)ev;
  const char *state = (e->state == XCB_COLORMAP_STATE_INSTALLED)   ? "Installed"
                     : (e->state == XCB_COLORMAP_STATE_UNINSTALLED) ? "Uninstalled"
                     : "Unknown";
  fprintf(out,
          "ColormapNotify:\n"
          "  window    = 0x%08x\n"
          "  colormap  = %u\n"
          "  new       = %u\n"
          "  state     = %s (%u)\n",
          e->window, e->colormap, e->_new, state, e->state);
  break;}
  case XCB_CLIENT_MESSAGE: {
    const xcb_client_message_event_t *e = (const xcb_client_message_event_t *)ev;
    /* print the first three fields that are common to every client‑message */
    fprintf(out,
            "ClientMessage:\n"
            "  format = %u   (bytes per element: %u)\n"
            "  window = 0x%08x\n"
            "  type   = %u\n",
            e->format,
            (e->format == 8)  ? 1 :
            (e->format == 16) ? 2 : 4,
            e->window,
            e->type);
    /* dump the data payload – the interpretation depends on the
       message’s ‘type’ atom, so we simply show the raw bytes.          */
    if (e->format == 8) {
        fprintf(out, "  data8 =");
        for (int i = 0; i < 20; ++i) {
            if (i % 8 == 0) fprintf(out, "\n    ");
            fprintf(out, "%02x ", e->data.data8[i]);
        }
    } else if (e->format == 16) {
        fprintf(out, "  data16 =");
        for (int i = 0; i < 10; ++i) {
            if (i % 8 == 0) fprintf(out, "\n    ");
            fprintf(out, "%04x ", e->data.data16[i]);
        }
    } else {    /* format == 32 */
        fprintf(out, "  data32 =");
        for (int i = 0; i < 5; ++i) {
            if (i % 8 == 0) fprintf(out, "\n    ");
            fprintf(out, "%08x ", e->data.data32[i]);
        }
    }
    fprintf(out, "\n");
    break;}
  case XCB_MAPPING_NOTIFY: { // 34
      const xcb_mapping_notify_event_t *e = (const xcb_mapping_notify_event_t *)ev;
      const char *request_str = (e->request == XCB_MAPPING_MODIFIER) ? "Modifier"
                              : (e->request == XCB_MAPPING_KEYBOARD) ? "Keyboard"
                              : (e->request == XCB_MAPPING_POINTER)  ? "Pointer"
                              : "Unknown";
      fprintf(out,
              "MappingNotify:\n"
              "  request       = %s (%u)\n"
              "  first_keycode = %u\n"
              "  count         = %u\n",
              request_str, e->request,
              e->first_keycode,
              e->count);
      break;}
  case XCB_GE_GENERIC: { // 35 generic extension event
    const xcb_ge_generic_event_t *e = (const xcb_ge_generic_event_t *)ev;
    fprintf(out,
            "GenericExtensionEvent:\n"
            "  extension     = %u\n"
            "  event_type    = %u\n"
            "  length        = %u (bytes)\n"
            "  full_sequence = %u\n",
            e->extension,
            e->event_type,
            e->length * 4,               /* length is in 4‑byte units */
            e->full_sequence);
      /* If you know the concrete extension that produced this event you can
         cast the payload (starting after the header) to the appropriate struct. */
    break;}
  default: fprintf(out, "Unknown core event (opcode %u)\n", evTyp);}
}
