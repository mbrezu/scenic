
# Ideas

 * there is a tree of layers, which are rendered in preorder;
 * there are two passes to establishing dimensions, 'measure' and
   'layout';
 * each widget has the following properties:
   * min-width/height;
   * max-width/height;
   * measured-width/height (from the 'measure' pass);
   * layout-left/top/width/height (from the 'layout' pass, relative to the scene - aka absolute);
   * parent - points to their parent widget;
 * there are container widgets that have an additional property,
   'children', that list the children;
 * each widget knows how to draw themselves;
 * there is no rendered image caching at the framework level for now;
   when rendering a frame, everything is drawn; if a widget takes a
   long time to draw itself, it should also take care of caching (save
   the last drawn image);
 * each event has listeners for the following events:
   - mouse button down/up;
   - mouse move/enter/leave;
   - key up/down;
   - got/lost focus;
 * events are propagated from the root of the widget hierarchy to the
   children; any widget can stop this;
 * events are propagated for each layer in the layer tree, in the
   drawing order for the layers; if propagation is stopped in a layer,
   it is not stopped for other layers (canceling the event only works
   inside a layer);
