(change-directory "~/Documents/GitHub/data-pages-YPP2/micro-graphics")
(load "../micro-graphics/load.lisp")

(setf w (mg:display-window))
(mg:set-param w :transform '(2 0 0 1 0 0))
(mg:draw-circle w 50 50 20)


