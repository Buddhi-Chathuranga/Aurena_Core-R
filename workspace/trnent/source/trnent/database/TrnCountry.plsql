-----------------------------------------------------------------------------
--
--  Logical unit: TrnCountry
--  Component:    TRNENT
--
--  IFS Developer Studio Template Version 3.0
--
--  Date    Sign    History
--  ------  ------  ---------------------------------------------------------
-----------------------------------------------------------------------------

layer Core;

-------------------- PUBLIC DECLARATIONS ------------------------------------


-------------------- PRIVATE DECLARATIONS -----------------------------------


-------------------- LU SPECIFIC IMPLEMENTATION METHODS ---------------------
@Override
PROCEDURE Raise_Record_Exist___ (
   rec_ IN trn_country_tab%ROWTYPE )
IS
BEGIN
   ERROR_SYS.Record_General(lu_name_,'ALLEXIT: The Trn Country [:p1] already exists',rec_.country_id);
   super(rec_);
   --Add post-processing code here
END Raise_Record_Exist___;

-------------------- LU SPECIFIC PRIVATE METHODS ----------------------------


-------------------- LU SPECIFIC PROTECTED METHODS --------------------------


-------------------- LU SPECIFIC PUBLIC METHODS -----------------------------

PROCEDURE New (
   new_attr_ IN VARCHAR2)
IS
   newrec_ trn_Country_tab%ROWTYPE;
   indrec_ Indicator_Rec;
   objid_               VARCHAR2(2000);
   objversion_          VARCHAR2(2000); 
   attr_ VARCHAR2(4000) := new_attr_;
BEGIN
   attr_ := new_attr_;
   Unpack___(newrec_, indrec_, attr_);
   Check_Insert___(newrec_, indrec_, attr_);
   Insert___(objid_, objversion_, newrec_, attr_);
END New;

