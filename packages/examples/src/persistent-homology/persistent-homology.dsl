
type Point
type Frames
type Connection

-- type Connection1
-- type Connection2
-- type Connection3

-- Connection1 <: Connection
-- Connection2 <: Connection
-- Connection3 <: Connection

constructor MakeConnection1(Point p1, Point p2) -> Connection
constructor MakeConnection2(Point p1, Point p2) -> Connection
constructor MakeConnection3(Point p1, Point p2) -> Connection

predicate ConnectedOnFrame1(Point p1, Point p2)
predicate ConnectedOnFrame2(Point p1, Point p2)
predicate ConnectedOnFrame3(Point p1, Point p2)

predicate NotConnectedOnFrame1(Point p1, Point p2)
predicate NotConnectedOnFrame2(Point p1, Point p2)
predicate NotConnectedOnFrame3(Point p1, Point p2)
