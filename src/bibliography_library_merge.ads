with Bibliography_Library; use Bibliography_Library;
with Bibentry; use Bibentry;

package Bibliography_Library_Merge is

   type Choice is (None, Left, Both, Right);

   generic
      with function Decide(left, right : Bibentry_Type ) return Choice;
   procedure Binary_Merge(Result      : out Bibliography_Library_Type;
                          Left, Right : in  Bibliography_Library_Type);

end Bibliography_Library_Merge;
