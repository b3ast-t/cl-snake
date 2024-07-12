(in-package :cl-snake)

(cffi:define-foreign-library raylib
  (:windows "raylib.dll"))

(unless (cffi:foreign-library-loaded-p 'raylib)
  (format t "loaded")
  (cffi:use-foreign-library raylib))

(cffi:defcfun ("InitWindow" %init-window) :void
  "Initialize window and OpenGL context"
  (width :int)
  (height :int)
  (title :string))

(cffi:defcfun ("CloseWindow" %close-window) :void
  "Close window and unload OpenGL context")

(cffi:defcfun ("SetTargetFPS" %set-target-fps) :void
  "Set target FPS (maximum)"
  (fps :int))

(cffi:defcfun ("WindowShouldClose" %window-should-close) :bool
  "Check if application should close (KEY_ESCAPE pressed or windows close icon clicked)")

(cffi:defcfun ("BeginDrawing" %begin-drawing) :void
  "Setup canvas (framebuffer) to start drawing")

(cffi:defcfun ("EndDrawing" %end-drawing) :void
  "End canvas drawing and swap buffers (double buffering)")

(cffi:defcstruct (%color :class color-type)
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))


(defstruct color
  r g b (a 255))

(defmethod cffi:translate-into-foreign-memory (value (type color-type) ptr)
  (cffi:with-foreign-slots ((r g b a) ptr (:struct %color))
    (setf r (color-r value)
	  g (color-g value)
	  b (color-b value)
	  a (color-a value))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod cffi:expand-into-foreign-memory (value (type color-type) ptr)
    `(cffi:with-foreign-slots ((r g b a) ,ptr (:struct %color))
       (setf r (color-r ,value)
	     g (color-g ,value)
	     b (color-b ,value)
	     a (color-a ,value)
	     ))))

(cffi:defcstruct  (%vec2 :class vec2-type)
  (x :float)
  (y :float))

(defstruct vec2
  (x (float 0) :type float)
  (y (float 0) :type float))

(defmethod cffi:translate-into-foreign-memory (value (type vec2-type) ptr)
  (cffi:with-foreign-slots ((x y) ptr (:struct %vec2))
    (setf x (vec2-x value)
	  y (vec2-y value))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod cffi:expand-into-foreign-memory (value (type vec2-type) ptr)
    `(cffi:with-foreign-slots ((x y) ,ptr (:struct %vec2))
       (setf x (vec2-x ,value)
	     y (vec2-y ,value)))))

(cffi:defcfun ("ClearBackground" %clear-background) :void
  "Set background color (framebuffer clear color)"
  (color (:struct %color)))

(cffi:defcfun ("DrawText" %draw-text) :void
  (text :string)
  (pos-x :int)
  (pos-y :int)
  (font-size :int)
  (color (:struct %color)))

(cffi:defcenum KeyboardKey
  (:key-null 0)
  (:key-apostrophe 39)
  (:key-comma 44)
  (:key-minus 45)
  (:key-period 46)
  (:key-slash 47)
  (:key-zero 48)
  (:key-one 49)
  (:key-two 50)
  (:key-three 51)
  (:key-four 52)
  (:key-five 53)
  (:key-six 54)
  (:key-seven 55)
  (:key-eight 56)
  (:key-nine 57)
  (:key-semicolon 59)
  (:key-equal 61)
  (:key-a 65)
  (:key-b 66)
  (:key-c 67)
  (:key-d 68)
  (:key-e 69)
  (:key-f 70)
  (:key-g 71)
  (:key-h 72)
  (:key-i 73)
  (:key-j 74)
  (:key-k 75)
  (:key-l 76)
  (:key-m 77)
  (:key-n 78)
  (:key-o 79)
  (:key-p 80)
  (:key-q 81)
  (:key-r 82)
  (:key-s 83)
  (:key-t 84)
  (:key-u 85)
  (:key-v 86)
  (:key-w 87)
  (:key-x 88)
  (:key-y 89)
  (:key-z 90)
  (:key-left-bracket 91)
  (:key-backslash 92)
  (:key-right-bracket 93)
  (:key-grave 96)

  (:key-space 32)
  (:key-escape 256)
  (:key-enter 257)
  (:key-tab 258)
  (:key-backspace 259)
  (:key-insert 260)
  (:key-delete 261)
  (:key-right 262)
  (:key-left 263)
  (:key-down 264)
  (:key-up 265)
  (:key-page-up 266)
  (:key-page-down 267)
  (:key-home 268)
  (:key-end 269)
  (:key-caps-lock 280)
  (:key-scroll-lock 281)
  (:key-num-lock 282)
  (:key-print-screen 283)
  (:key-pause 284)
  (:key-f1 290)
  (:key-f2 291)
  (:key-f3 292)
  (:key-f4 293)
  (:key-f5 294)
  (:key-f6 295)
  (:key-f7 296)
  (:key-f8 297)
  (:key-f9 298)
  (:key-f10 299)
  (:key-f11 300)
  (:key-f12 301)
  (:key-left-shift 340)
  (:key-left-control 341)
  (:key-left-alt 342)
  (:key-left-super 343)
  (:key-right-shift 344)
  (:key-right-control 345)
  (:key-right-alt 346)
  (:key-right-super 347)
  (:key-kb-menu 348)

  (:key-kp-0 320)
  (:key-kp-1 321)
  (:key-kp-2 322)
  (:key-kp-3 323)
  (:key-kp-4 324)
  (:key-kp-5 325)
  (:key-kp-6 326)
  (:key-kp-7 327)
  (:key-kp-8 328)
  (:key-kp-9 329)
  (:key-kp-decimal 330)
  (:key-kp-divide 331)
  (:key-kp-multiply 332)
  (:key-kp-subtract 333)
  (:key-kp-add 334)
  (:key-kp-enter 335)
  (:key-kp-equal 336)

  (:key-back 4)
  (:key-menu 82)
  (:key-volume-up 24)
  (:key-volume-down 25))

(cffi:defcfun ("IsKeyPressed" %is-key-pressed) :bool
  (key-code keyboardkey))

(cffi:defcfun ("DrawLineV" %draw-line-v) :void
  (start (:struct %vec2))
  (end (:struct %vec2))
  (color (:struct %color)))

(cffi:defcfun ("DrawRectangleV" %draw-rectangle-v) :void
  (position (:struct %vec2))
  (size (:struct %vec2))
  (color (:struct %color)))

(cffi:defcfun ("MeasureText" %measure-text) :int
  (text :string)
  (font-size :int))

(cffi:defcfun ("GetScreenWidth" %get-screen-width) :int)
(cffi:defcfun ("GetScreenHeight" %get-screen-height) :int)

