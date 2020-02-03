// On porting:

// If you’re using single instead of double precision in JS, you won’t be able to reach the default tolerance ε that I set in the code.  But then you can set the tolerance to something bigger like 10^-3 and the results you get will be basically indistinguishable (I just checked in the C++ version).  I was just being aggressive about the C++ implementation to be 100% sure that I got everything right.

// Otherwise, can’t think of any differences between C++ and JS (but I’ve been surprised in the past!!)

// These routines optimize a planar triangle mesh so that
// the edge lengths and areas of each triangle are as close
// as possible to those of equilateral triangles.  Specifically,
// if f_i is the location of vertex i, it minimizes the energy
//
//    \sum_{ij \in E} (|f_j-f_i|^2 - L)^2 +
//    \sum_{ijk \in F} ((f_j-f_i)x(f_k-f_i) - A)^2,
//
// where L is the target edge length, and A is (twice) the area
// of an equilateral triangle with edge length L.  Since all
// coordinates are in 2D, the cross product u x v here denotes
// the scalar quantity u_1 v_2 - u_2 v_1.
//
// Optimization is performed via gradient descent with backtracking
// line search.  The algorithm is broken into two routines: one
// which evaluates the energy, and another which performs gradient
// descent.  A reasonable choice for the parameter L is to use the
// average edge length of the input mesh.
//
// Overall the algorithm should be fairly robust and produce nice-
// looking results, though there is no guarantee that the mapping will
// be injective---for instance, in corner cases one can get triangles
// that wind around a vertex multiple times.

   double energy( const Mesh& mesh, double L )
   {
      // initialize energy to zero
      double E = 0.;

      // add contribution of edge length energy
      for( Edge& e : mesh.edges )
      {
         VertexIter v1 = e.he->vertex;
         VertexIter v2 = e.he->flip->vertex;
         Vector u = v1->position - v2->position;
         E += sqr( u.norm2() - L )/4.; // sqr is square, not square root
      }

      // add contribution of area energy
      const double A0 = L * sqrt(3.)/2.; // sqrt is square root, not square
      for( Face& f : mesh.faces )
      {
         VertexIter va = f.he->vertex;
         VertexIter vb = f.he->next->vertex;
         VertexIter vc = f.he->next->next->vertex;
         Vector& a = va->position;
         Vector& b = vb->position;
         Vector& c = vc->position;

         double A = cross( b-a, c-a ).z;

         E += sqr( A0 - A )/2.;
      }

      return E;
   }

   void optimizeMesh( const Mesh& mesh )
   {
      // parameters for energy
      double L = mesh.meanEdgeLength(); // target edge length---note that we shouldn't compute this within energh() since the mean edge length will change throughout optimization
      const double A0 = L * sqrt(3.)/2.; // (twice) the area of corresponding equilateral triangle

      // parameters for gradient descent
      const int maxGradientSteps = 10000;
      const double epsilon = 1e-6; // stopping tolerance

      // take gradient descent steps
      int step;
      for( step = 0; step < maxGradientSteps; step++ )
      {
         // Part 1: Evaluate the gradient =========================
         
         // initialize gradient to zero
         for( Vertex& v : mesh.vertices )
         {
            v.gradient = Vector( 0., 0., 0. );
         }

         // add gradient of edge length energy
         for( Edge& e : mesh.edges )
         {
            // get endpoints of edge e = (v1,v2)
            VertexIter v1 = e.he->vertex;
            VertexIter v2 = e.he->flip->vertex;

            // compute edge vector
            Vector u = v1->position - v2->position;

            // add gradient contribution at each endpoint
            v1->gradient += (u.norm2()-L)*u;
            v2->gradient -= (u.norm2()-L)*u;
         }

         // add gradient of (signed) triangle area energy
         for( Face& f : mesh.faces )
         {
            // get vertices of triangle f = (va,vb,vc)
            VertexIter va = f.he->vertex;
            VertexIter vb = f.he->next->vertex;
            VertexIter vc = f.he->next->next->vertex;

            // get positions of the three vertices
            Vector& a = va->position;
            Vector& b = vb->position;
            Vector& c = vc->position;

            // compute (twice) the triangle area
            double A = cross( b-a, c-a ).z;

            // add gradient contribution at vertex a
            va->gradient.x += (A0-A)*(c.y-b.y);
            va->gradient.y += (A0-A)*(b.x-c.x);

            // add gradient contribution at vertex b
            vb->gradient.x += (A0-A)*(a.y-c.y);
            vb->gradient.y += (A0-A)*(c.x-a.x);

            // add gradient contribution at vertex c
            vc->gradient.x += (A0-A)*(b.y-a.y);
            vc->gradient.y += (A0-A)*(a.x-b.x);
         }
         
         // Part 2: Take a gradient step =========================

         double E0 = energy( mesh, L ); // get the energy of the current configuration

         // compute the squared norm of the gradient
         double gradNormSquared = 0.;
         for( Vertex& v : mesh.vertices )
         {
            v.originalPosition = v.position;
            gradNormSquared += v.gradient.norm2();
         }

         // perform backtracking line search, as described in
         // Body & Vandenberghe, "Convex Optimization" (Algorithm 9.2)
         double tau = 1.; // time step
         const double alpha = 0.25; // sufficient decrease parameter
         const double beta = 0.5; // backtracking parameter
         int nMaxBacktrackingSteps = 100;
         for( int i = 0; i < nMaxBacktrackingSteps; i++ )
         {
            // move to tentative new configuration
            for( Vertex& v : mesh.vertices )
            {
               v.position = v.originalPosition - tau * v.gradient;
            }

            // evaluate energy at new configuration
            double E = energy( mesh, L );

            // if there was a sufficient decrease in energy, stop
            if( E < E0 - alpha*tau*gradNormSquared ) break;

            // otherwise, shrink the time step
            tau *= beta;
         }

         // if the gradient is small enough, we're at a local minimum and can stop
         if( sqrt(gradNormSquared) < epsilon ) break;
      }

      if( step == maxGradientSteps )
      {
         cerr << "Warning: did not reach requested tolerance " << epsilon << " after " << step << " steps." << endl;
      }
      else
      {
         cerr << "Converged to a tolerance of " << epsilon << " after " << step << " steps." << endl;
      }
   }
