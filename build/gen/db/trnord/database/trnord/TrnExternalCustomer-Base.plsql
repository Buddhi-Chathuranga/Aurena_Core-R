-----------------------------------------------------------------------------
--
--  Logical unit: TrnExternalCustomer
--  Component:    TRNORD
--
--  Template:     3.0
--  Built by:     IFS Developer Studio (unit-test)
--
--  NOTE! Do not edit!! This file is completely generated and will be
--        overwritten next time the corresponding model is saved.
-----------------------------------------------------------------------------

layer Base;

@AllowTableOrViewAccess TRN_CUSTOMER_TAB

-------------------- PUBLIC DECLARATIONS ------------------------------------

--TYPE Primary_Key_Rec IS RECORD
--  (company_id                     TRN_CUSTOMER_TAB.company_id%TYPE,
--   customer_id                    TRN_CUSTOMER_TAB.customer_id%TYPE);

TYPE Public_Rec IS RECORD
  (company_id                     TRN_CUSTOMER_TAB.company_id%TYPE,
   customer_id                    TRN_CUSTOMER_TAB.customer_id%TYPE,
   "rowid"                        rowid,
   rowversion                     TRN_CUSTOMER_TAB.rowversion%TYPE,
   rowkey                         TRN_CUSTOMER_TAB.rowkey%TYPE,
   rowtype                        TRN_CUSTOMER_TAB.rowtype%TYPE,
   name                           TRN_CUSTOMER_TAB.name%TYPE,
   credit_limit                   TRN_CUSTOMER_TAB.credit_limit%TYPE,
   discount                       TRN_CUSTOMER_TAB.discount%TYPE,
   active                         TRN_CUSTOMER_TAB.active%TYPE,
   branch_id                      TRN_CUSTOMER_TAB.branch_id%TYPE,
   preferred_inventory_id         TRN_CUSTOMER_TAB.preferred_inventory_id%TYPE);




-------------------- PRIVATE DECLARATIONS -----------------------------------

TYPE Indicator_Rec IS RECORD
  (company_id                     BOOLEAN := FALSE,
   customer_id                    BOOLEAN := FALSE,
   name                           BOOLEAN := FALSE,
   credit_limit                   BOOLEAN := FALSE,
   discount                       BOOLEAN := FALSE,
   active                         BOOLEAN := FALSE,
   comments                       BOOLEAN := FALSE,
   branch_id                      BOOLEAN := FALSE,
   preferred_inventory_id         BOOLEAN := FALSE);

-------------------- BASE METHODS -------------------------------------------

-- Key_Message___
--    Returns an error message containing the keys.
FUNCTION Key_Message___ (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN VARCHAR2 
IS
   msg_ VARCHAR2(4000) := Message_SYS.Construct('ERROR_KEY');
BEGIN
   Message_SYS.Add_Attribute(msg_, 'COMPANY_ID', company_id_);
   Message_SYS.Add_Attribute(msg_, 'CUSTOMER_ID', customer_id_);
   RETURN msg_;
END Key_Message___;

-- Formatted_Key___
--    Returns an error string containing the keys.
FUNCTION Formatted_Key___ (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN VARCHAR2
IS
   formatted_key_ VARCHAR2(4000) := Language_SYS.Translate_Item_Prompt_(lu_name_, 'COMPANY_ID', Fnd_Session_API.Get_Language) || ': ' || company_id_ || ', ' ||
                                    Language_SYS.Translate_Item_Prompt_(lu_name_, 'CUSTOMER_ID', Fnd_Session_API.Get_Language) || ': ' || customer_id_;
BEGIN
   RETURN formatted_key_;
END Formatted_Key___;

-- Raise_Too_Many_Rows___
--    Raises error for: More then one row found for a single key.
PROCEDURE Raise_Too_Many_Rows___ (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER,
   methodname_ IN VARCHAR2 )
IS
BEGIN
   Error_SYS.Set_Key_Values(Key_Message___(company_id_, customer_id_),
                            Formatted_Key___(company_id_, customer_id_));
   Error_SYS.Fnd_Too_Many_Rows(Trn_External_Customer_API.lu_name_, NULL, methodname_);
END Raise_Too_Many_Rows___;


-- Raise_Record_Not_Exist___
--    Raises error for: No data found for given key.
PROCEDURE Raise_Record_Not_Exist___ (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER )
IS
BEGIN
   Error_SYS.Set_Key_Values(Key_Message___(company_id_, customer_id_),
                            Formatted_Key___(company_id_, customer_id_));
   Error_SYS.Fnd_Record_Not_Exist(Trn_External_Customer_API.lu_name_);
END Raise_Record_Not_Exist___;


-- Raise_Record_Exist___
--    Raises error for: Data with given key value already exist.
PROCEDURE Raise_Record_Exist___ (
   rec_ IN trn_customer_tab%ROWTYPE )
IS
BEGIN
   Error_SYS.Set_Key_Values(Key_Message___(rec_.company_id, rec_.customer_id),
                            Formatted_Key___(rec_.company_id, rec_.customer_id));
   Error_SYS.Fnd_Record_Exist(Trn_External_Customer_API.lu_name_);
END Raise_Record_Exist___;

-- Raise_Constraint_Violated___
--    Raises error for: Data with given value for constraint that already exist.
--    constraint_ contains the violated constraint to be used when overriding the method.
PROCEDURE Raise_Constraint_Violated___ (
   rec_ IN trn_customer_tab%ROWTYPE,
   constraint_ IN VARCHAR2 )
IS
BEGIN
   Error_SYS.Fnd_Record_Exist(Trn_External_Customer_API.lu_name_);
END Raise_Constraint_Violated___;


-- Raise_Item_Format___
--    Raises error for: Data value format is incorrect.
PROCEDURE Raise_Item_Format___ (
   name_ IN VARCHAR2,
   value_ IN VARCHAR2 )
IS
BEGIN
   Error_SYS.Fnd_Item_Format(Trn_External_Customer_API.lu_name_, name_, value_);
END Raise_Item_Format___;

-- Raise_Record_Modified___
--    Raises error for: The database row is newer then the current.
PROCEDURE Raise_Record_Modified___ (
   rec_ IN trn_customer_tab%ROWTYPE )
IS
BEGIN
   Error_SYS.Set_Key_Values(Key_Message___(rec_.company_id, rec_.customer_id),
                            Formatted_Key___(rec_.company_id, rec_.customer_id));
   Error_SYS.Fnd_Record_Modified(Trn_External_Customer_API.lu_name_);
END Raise_Record_Modified___;


-- Raise_Record_Locked___
--    Raises error for: The database row is already locked.
PROCEDURE Raise_Record_Locked___ (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER )
IS
BEGIN
   Error_SYS.Set_Key_Values(Key_Message___(company_id_, customer_id_),
                            Formatted_Key___(company_id_, customer_id_));
   Error_SYS.Fnd_Record_Locked(Trn_External_Customer_API.lu_name_);
END Raise_Record_Locked___;


-- Raise_Record_Removed___
--    Raises error for: The database row is no longer present.
PROCEDURE Raise_Record_Removed___ (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER )
IS
BEGIN
   Error_SYS.Set_Key_Values(Key_Message___(company_id_, customer_id_),
                            Formatted_Key___(company_id_, customer_id_));
   Error_SYS.Fnd_Record_Removed(Trn_External_Customer_API.lu_name_);
END Raise_Record_Removed___;


-- Lock_By_Id___
--    Locks a database row based on the objid and objversion.
FUNCTION Lock_By_Id___ (
   objid_      IN VARCHAR2,
   objversion_ IN VARCHAR2 ) RETURN trn_customer_tab%ROWTYPE
IS
   row_locked  EXCEPTION;
   PRAGMA      EXCEPTION_INIT(row_locked, -0054);
   rec_        trn_customer_tab%ROWTYPE;
BEGIN
   SELECT *
      INTO  rec_
      FROM  trn_customer_tab
      WHERE rowid = objid_
      AND    to_char(rowversion,'YYYYMMDDHH24MISS') = objversion_
      FOR UPDATE NOWAIT;
   RETURN rec_;
EXCEPTION
   WHEN row_locked THEN
      Error_SYS.Fnd_Record_Locked(lu_name_);
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(NULL, NULL, 'Lock_By_Id___');
   WHEN no_data_found THEN
      BEGIN
         SELECT *
            INTO  rec_
            FROM  trn_customer_tab
            WHERE rowid = objid_;
         Raise_Record_Modified___(rec_);
      EXCEPTION
         WHEN no_data_found THEN
            Error_SYS.Fnd_Record_Removed(lu_name_);
         WHEN too_many_rows THEN
            Raise_Too_Many_Rows___(NULL, NULL, 'Lock_By_Id___');
      END;
END Lock_By_Id___;


-- Lock_By_Keys___
--    Locks a database row based on the primary key values.
--    Waits until record released if locked by another session.
FUNCTION Lock_By_Keys___ (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER) RETURN trn_customer_tab%ROWTYPE
IS
   rec_        trn_customer_tab%ROWTYPE;
BEGIN
   BEGIN
      SELECT *
         INTO  rec_
         FROM  trn_customer_tab
         WHERE company_id = company_id_
         AND   customer_id = customer_id_
         AND   rowtype LIKE '%TrnExternalCustomer'
         FOR UPDATE;
      RETURN rec_;
   EXCEPTION
      WHEN no_data_found THEN
         Raise_Record_Removed___(company_id_, customer_id_);
      WHEN too_many_rows THEN
         Raise_Too_Many_Rows___(company_id_, customer_id_, 'Lock_By_Keys___');
   END;
END Lock_By_Keys___;


-- Lock_By_Keys_Nowait___
--    Locks a database row based on the primary key values.
--    Raises exception if row already locked.
FUNCTION Lock_By_Keys_Nowait___ (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER) RETURN trn_customer_tab%ROWTYPE
IS
   row_locked  EXCEPTION;
   PRAGMA      EXCEPTION_INIT(row_locked, -0054);
   rec_        trn_customer_tab%ROWTYPE;
BEGIN
   BEGIN
      SELECT *
         INTO  rec_
         FROM  trn_customer_tab
         WHERE company_id = company_id_
         AND   customer_id = customer_id_
         AND   rowtype LIKE '%TrnExternalCustomer'
         FOR UPDATE NOWAIT;
      RETURN rec_;
   EXCEPTION
      WHEN row_locked THEN
         Raise_Record_Locked___(company_id_, customer_id_);
      WHEN too_many_rows THEN
         Raise_Too_Many_Rows___(company_id_, customer_id_, 'Lock_By_Keys___');
      WHEN no_data_found THEN
         Raise_Record_Removed___(company_id_, customer_id_);
   END;
END Lock_By_Keys_Nowait___;


-- Get_Object_By_Id___
--    Fetched a database row based on given the objid.
FUNCTION Get_Object_By_Id___ (
   objid_ IN VARCHAR2 ) RETURN trn_customer_tab%ROWTYPE
IS
   lu_rec_ trn_customer_tab%ROWTYPE;
BEGIN
   SELECT *
      INTO  lu_rec_
      FROM  trn_customer_tab
      WHERE rowid = objid_;
   RETURN lu_rec_;
EXCEPTION
   WHEN no_data_found THEN
      Error_SYS.Fnd_Record_Removed(lu_name_);
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(NULL, NULL, 'Get_Object_By_Id___');
END Get_Object_By_Id___;


-- Get_Object_By_Keys___
--    Fetched a database row based on given the primary key values.
@UncheckedAccess
FUNCTION Get_Object_By_Keys___ (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN trn_customer_tab%ROWTYPE
IS
   lu_rec_ trn_customer_tab%ROWTYPE;
BEGIN
   SELECT *
      INTO  lu_rec_
      FROM  trn_customer_tab
      WHERE company_id = company_id_
      AND   customer_id = customer_id_
      AND   rowtype LIKE '%TrnExternalCustomer';
   RETURN lu_rec_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN lu_rec_;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Get_Object_By_Keys___');
END Get_Object_By_Keys___;


-- Check_Exist___
--    Checks if a database row is already stored based on the primary key values.
@UncheckedAccess
FUNCTION Check_Exist___ (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN BOOLEAN
IS
   dummy_ NUMBER;
BEGIN
   IF (company_id_ IS NULL OR customer_id_ IS NULL) THEN
      RETURN FALSE;
   END IF;
   SELECT 1
      INTO  dummy_
      FROM  trn_customer_tab
      WHERE company_id = company_id_
      AND   customer_id = customer_id_
      AND   rowtype LIKE '%TrnExternalCustomer';
   RETURN TRUE;
EXCEPTION
   WHEN no_data_found THEN
      RETURN FALSE;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Check_Exist___');
END Check_Exist___;





-- Get_Version_By_Id___
--    Fetched the objversion for a database row based on the objid.
PROCEDURE Get_Version_By_Id___ (
   objid_      IN     VARCHAR2,
   objversion_ IN OUT VARCHAR2 )
IS
BEGIN
   SELECT to_char(rowversion,'YYYYMMDDHH24MISS')
      INTO  objversion_
      FROM  trn_customer_tab
      WHERE rowid = objid_;
EXCEPTION
   WHEN no_data_found THEN
      objversion_ := NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(NULL, NULL, 'Get_Version_By_Id___');
END Get_Version_By_Id___;


-- Get_Version_By_Keys___
--    Fetched the objversion for a database row based on the primary key.
PROCEDURE Get_Id_Version_By_Keys___ (
   objid_      IN OUT VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER )
IS
BEGIN
   SELECT rowid, to_char(rowversion,'YYYYMMDDHH24MISS')
      INTO  objid_, objversion_
      FROM  trn_customer_tab
      WHERE company_id = company_id_
      AND   customer_id = customer_id_
      AND   rowtype LIKE '%TrnExternalCustomer';
EXCEPTION
   WHEN no_data_found THEN
      objid_      := NULL;
      objversion_ := NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Get_Id_Version_By_Keys___');
END Get_Id_Version_By_Keys___;


-- Unpack___
--   Reads an attribute string and unpacks its contents into a record.
PROCEDURE Unpack___ (
   newrec_   IN OUT trn_customer_tab%ROWTYPE,
   indrec_   IN OUT Indicator_Rec,
   attr_     IN OUT VARCHAR2 )
IS
   ptr_   NUMBER;
   name_  VARCHAR2(30);
   value_ VARCHAR2(32000);
   msg_   VARCHAR2(32000);
BEGIN
   Reset_Indicator_Rec___(indrec_);
   Client_SYS.Clear_Attr(msg_);
   ptr_ := NULL;
   WHILE (Client_SYS.Get_Next_From_Attr(attr_, ptr_, name_, value_)) LOOP
      CASE name_
      WHEN ('COMPANY_ID') THEN
         newrec_.company_id := value_;
         indrec_.company_id := TRUE;
      WHEN ('CUSTOMER_ID') THEN
         newrec_.customer_id := Client_SYS.Attr_Value_To_Number(value_);
         indrec_.customer_id := TRUE;
      WHEN ('NAME') THEN
         newrec_.name := value_;
         indrec_.name := TRUE;
      WHEN ('CREDIT_LIMIT') THEN
         newrec_.credit_limit := Client_SYS.Attr_Value_To_Number(value_);
         indrec_.credit_limit := TRUE;
      WHEN ('DISCOUNT') THEN
         newrec_.discount := Client_SYS.Attr_Value_To_Number(value_);
         indrec_.discount := TRUE;
      WHEN ('ACTIVE') THEN
         newrec_.active := Fnd_Boolean_API.Encode(value_);
         IF (value_ IS NOT NULL AND newrec_.active IS NULL) THEN
            RAISE value_error;
         END IF;
         indrec_.active := TRUE;
      WHEN ('ACTIVE_DB') THEN
         newrec_.active := value_;
         indrec_.active := TRUE;
      WHEN ('COMMENTS') THEN
         newrec_.comments := value_;
         indrec_.comments := TRUE;
      WHEN ('BRANCH_ID') THEN
         newrec_.branch_id := value_;
         indrec_.branch_id := TRUE;
      WHEN ('PREFERRED_INVENTORY_ID') THEN
         newrec_.preferred_inventory_id := Client_SYS.Attr_Value_To_Number(value_);
         indrec_.preferred_inventory_id := TRUE;
      ELSE
         Client_SYS.Add_To_Attr(name_, value_, msg_);
      END CASE;
   END LOOP;
   attr_ := msg_;
EXCEPTION
   WHEN value_error THEN
      Raise_Item_Format___(name_, value_);
END Unpack___;


-- Pack___
--   Reads a record and packs its contents into an attribute string.
--   This is intended to be the reverse of Unpack___
FUNCTION Pack___ (
   rec_ IN trn_customer_tab%ROWTYPE ) RETURN VARCHAR2
IS
   attr_ VARCHAR2(32000);
BEGIN
   Client_SYS.Clear_Attr(attr_);
   IF (rec_.company_id IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('COMPANY_ID', rec_.company_id, attr_);
   END IF;
   IF (rec_.customer_id IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('CUSTOMER_ID', rec_.customer_id, attr_);
   END IF;
   IF (rec_.name IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('NAME', rec_.name, attr_);
   END IF;
   IF (rec_.credit_limit IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('CREDIT_LIMIT', rec_.credit_limit, attr_);
   END IF;
   IF (rec_.discount IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('DISCOUNT', rec_.discount, attr_);
   END IF;
   IF (rec_.active IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('ACTIVE', Fnd_Boolean_API.Decode(rec_.active), attr_);
      Client_SYS.Add_To_Attr('ACTIVE_DB', rec_.active, attr_);
   END IF;
   IF (rec_.comments IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('COMMENTS', rec_.comments, attr_);
   END IF;
   IF (rec_.branch_id IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('BRANCH_ID', rec_.branch_id, attr_);
   END IF;
   IF (rec_.preferred_inventory_id IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('PREFERRED_INVENTORY_ID', rec_.preferred_inventory_id, attr_);
   END IF;
   RETURN attr_;
END Pack___;


FUNCTION Pack___ (
   rec_ IN trn_customer_tab%ROWTYPE,
   indrec_ IN Indicator_Rec ) RETURN VARCHAR2
IS
   attr_ VARCHAR2(32000);
BEGIN
   Client_SYS.Clear_Attr(attr_);
   IF (indrec_.company_id) THEN
      Client_SYS.Add_To_Attr('COMPANY_ID', rec_.company_id, attr_);
   END IF;
   IF (indrec_.customer_id) THEN
      Client_SYS.Add_To_Attr('CUSTOMER_ID', rec_.customer_id, attr_);
   END IF;
   IF (indrec_.name) THEN
      Client_SYS.Add_To_Attr('NAME', rec_.name, attr_);
   END IF;
   IF (indrec_.credit_limit) THEN
      Client_SYS.Add_To_Attr('CREDIT_LIMIT', rec_.credit_limit, attr_);
   END IF;
   IF (indrec_.discount) THEN
      Client_SYS.Add_To_Attr('DISCOUNT', rec_.discount, attr_);
   END IF;
   IF (indrec_.active) THEN
      Client_SYS.Add_To_Attr('ACTIVE', Fnd_Boolean_API.Decode(rec_.active), attr_);
      Client_SYS.Add_To_Attr('ACTIVE_DB', rec_.active, attr_);
   END IF;
   IF (indrec_.comments) THEN
      Client_SYS.Add_To_Attr('COMMENTS', rec_.comments, attr_);
   END IF;
   IF (indrec_.branch_id) THEN
      Client_SYS.Add_To_Attr('BRANCH_ID', rec_.branch_id, attr_);
   END IF;
   IF (indrec_.preferred_inventory_id) THEN
      Client_SYS.Add_To_Attr('PREFERRED_INVENTORY_ID', rec_.preferred_inventory_id, attr_);
   END IF;
   RETURN attr_;
END Pack___;



-- Pack_Table___
--   Reads a record and packs its contents into an attribute string.
--   Similar to Pack___ but just uses table column names and DB values
FUNCTION Pack_Table___ (
   rec_ IN trn_customer_tab%ROWTYPE ) RETURN VARCHAR2
IS
   attr_ VARCHAR2(32000);
BEGIN
   Client_SYS.Clear_Attr(attr_);
   Client_SYS.Add_To_Attr('COMPANY_ID', rec_.company_id, attr_);
   Client_SYS.Add_To_Attr('CUSTOMER_ID', rec_.customer_id, attr_);
   Client_SYS.Add_To_Attr('NAME', rec_.name, attr_);
   Client_SYS.Add_To_Attr('CREDIT_LIMIT', rec_.credit_limit, attr_);
   Client_SYS.Add_To_Attr('DISCOUNT', rec_.discount, attr_);
   Client_SYS.Add_To_Attr('ACTIVE', rec_.active, attr_);
   Client_SYS.Add_To_Attr('COMMENTS', rec_.comments, attr_);
   Client_SYS.Add_To_Attr('BRANCH_ID', rec_.branch_id, attr_);
   Client_SYS.Add_To_Attr('PREFERRED_INVENTORY_ID', rec_.preferred_inventory_id, attr_);
   Client_SYS.Add_To_Attr('ROWKEY', rec_.rowkey, attr_);
   RETURN attr_;
END Pack_Table___;



-- Public_To_Table___
--   Reads values in the public_rec record and returns them in a table rowtype record.
FUNCTION Public_To_Table___ (
   public_ IN Public_Rec ) RETURN trn_customer_tab%ROWTYPE
IS
   rec_ trn_customer_tab%ROWTYPE;
BEGIN
   rec_.rowversion                     := public_.rowversion;
   rec_.rowkey                         := public_.rowkey;
   rec_.rowtype                        := public_.rowtype;
   rec_.company_id                     := public_.company_id;
   rec_.customer_id                    := public_.customer_id;
   rec_.name                           := public_.name;
   rec_.credit_limit                   := public_.credit_limit;
   rec_.discount                       := public_.discount;
   rec_.active                         := public_.active;
   rec_.branch_id                      := public_.branch_id;
   rec_.preferred_inventory_id         := public_.preferred_inventory_id;
   RETURN rec_;
END Public_To_Table___;



-- Table_To_Public___
--   Reads values in the table rowtype record and returns them in a public_rec record.
FUNCTION Table_To_Public___ (
   rec_ IN trn_customer_tab%ROWTYPE ) RETURN Public_Rec
IS
   public_ Public_Rec;
BEGIN
   public_.rowversion                     := rec_.rowversion;
   public_.rowkey                         := rec_.rowkey;
   public_.rowtype                        := rec_.rowtype;
   public_.company_id                     := rec_.company_id;
   public_.customer_id                    := rec_.customer_id;
   public_.name                           := rec_.name;
   public_.credit_limit                   := rec_.credit_limit;
   public_.discount                       := rec_.discount;
   public_.active                         := rec_.active;
   public_.branch_id                      := rec_.branch_id;
   public_.preferred_inventory_id         := rec_.preferred_inventory_id;
   RETURN public_;
END Table_To_Public___;


-- Reset_Indicator_Rec___
--   Resets all elements of given Indicator_Rec to FALSE.
PROCEDURE Reset_Indicator_Rec___ (
   indrec_ IN OUT Indicator_Rec )
IS
   empty_indrec_ Indicator_Rec;
BEGIN
   indrec_ := empty_indrec_;
END Reset_Indicator_Rec___;


-- Get_Indicator_Rec___
--   Returns an Indicator_Rec that reflects the content of a table record.
FUNCTION Get_Indicator_Rec___ (
   rec_ IN trn_customer_tab%ROWTYPE ) RETURN Indicator_Rec
IS
   indrec_ Indicator_Rec;
BEGIN
   indrec_.company_id := rec_.company_id IS NOT NULL;
   indrec_.customer_id := rec_.customer_id IS NOT NULL;
   indrec_.name := rec_.name IS NOT NULL;
   indrec_.credit_limit := rec_.credit_limit IS NOT NULL;
   indrec_.discount := rec_.discount IS NOT NULL;
   indrec_.active := rec_.active IS NOT NULL;
   indrec_.comments := rec_.comments IS NOT NULL;
   indrec_.branch_id := rec_.branch_id IS NOT NULL;
   indrec_.preferred_inventory_id := rec_.preferred_inventory_id IS NOT NULL;
   RETURN indrec_;
END Get_Indicator_Rec___;


-- Get_Indicator_Rec___
--   Returns an Indicator_Rec that reflects the difference between two table records.
FUNCTION Get_Indicator_Rec___ (
   oldrec_ IN trn_customer_tab%ROWTYPE,
   newrec_ IN trn_customer_tab%ROWTYPE ) RETURN Indicator_Rec
IS
   indrec_ Indicator_Rec;
BEGIN
   indrec_.company_id := Validate_SYS.Is_Changed(oldrec_.company_id, newrec_.company_id);
   indrec_.customer_id := Validate_SYS.Is_Changed(oldrec_.customer_id, newrec_.customer_id);
   indrec_.name := Validate_SYS.Is_Changed(oldrec_.name, newrec_.name);
   indrec_.credit_limit := Validate_SYS.Is_Changed(oldrec_.credit_limit, newrec_.credit_limit);
   indrec_.discount := Validate_SYS.Is_Changed(oldrec_.discount, newrec_.discount);
   indrec_.active := Validate_SYS.Is_Changed(oldrec_.active, newrec_.active);
   indrec_.comments := Validate_SYS.Is_Changed(oldrec_.comments, newrec_.comments);
   indrec_.branch_id := Validate_SYS.Is_Changed(oldrec_.branch_id, newrec_.branch_id);
   indrec_.preferred_inventory_id := Validate_SYS.Is_Changed(oldrec_.preferred_inventory_id, newrec_.preferred_inventory_id);
   RETURN indrec_;
END Get_Indicator_Rec___;


-- Check_Common___
--   Perform validations on a record, that should be done for both insert and delete.
PROCEDURE Check_Common___ (
   oldrec_ IN     trn_customer_tab%ROWTYPE,
   newrec_ IN OUT trn_customer_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
BEGIN
   IF (newrec_.active IS NOT NULL)
   AND (indrec_.active)
   AND (Validate_SYS.Is_Changed(oldrec_.active, newrec_.active)) THEN
      Fnd_Boolean_API.Exist_Db(newrec_.active);
   END IF;
   IF (newrec_.company_id IS NOT NULL)
   AND (indrec_.company_id)
   AND (Validate_SYS.Is_Changed(oldrec_.company_id, newrec_.company_id)) THEN
      Trn_Company_API.Exist(newrec_.company_id);
   END IF;
   IF (newrec_.company_id IS NOT NULL AND newrec_.branch_id IS NOT NULL AND newrec_.preferred_inventory_id IS NOT NULL)
   AND (indrec_.company_id OR indrec_.branch_id OR indrec_.preferred_inventory_id)
   AND (Validate_SYS.Is_Changed(oldrec_.company_id, newrec_.company_id)
     OR Validate_SYS.Is_Changed(oldrec_.branch_id, newrec_.branch_id)
     OR Validate_SYS.Is_Changed(oldrec_.preferred_inventory_id, newrec_.preferred_inventory_id)) THEN
      Trn_Inventory_API.Exist(newrec_.company_id, newrec_.branch_id, newrec_.preferred_inventory_id);
   END IF;
   Error_SYS.Check_Not_Null(lu_name_, 'COMPANY_ID', newrec_.company_id);
   Error_SYS.Check_Not_Null(lu_name_, 'CUSTOMER_ID', newrec_.customer_id);
   Error_SYS.Check_Not_Null(lu_name_, 'NAME', newrec_.name);
   Error_SYS.Check_Not_Null(lu_name_, 'CREDIT_LIMIT', newrec_.credit_limit);
   Error_SYS.Check_Not_Null(lu_name_, 'DISCOUNT', newrec_.discount);
   Error_SYS.Check_Not_Null(lu_name_, 'ACTIVE', newrec_.active);
END Check_Common___;


-- Prepare_Insert___
--   Set client default values into an attribute string.
PROCEDURE Prepare_Insert___ (
   attr_ IN OUT VARCHAR2 )
IS
BEGIN
   Client_SYS.Clear_Attr(attr_);
END Prepare_Insert___;


-- Check_Insert___
--   Perform validations on a new record before it is insert.
PROCEDURE Check_Insert___ (
   newrec_ IN OUT trn_customer_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
   oldrec_ trn_customer_tab%ROWTYPE;
BEGIN
   Check_Common___(oldrec_, newrec_, indrec_, attr_);
END Check_Insert___;


-- Insert___
--   Insert a record to the database.
PROCEDURE Insert___ (
   objid_      OUT    VARCHAR2,
   objversion_ OUT    VARCHAR2,
   newrec_     IN OUT trn_customer_tab%ROWTYPE,
   attr_       IN OUT VARCHAR2 )
IS
   value_too_large  EXCEPTION;
   PRAGMA           EXCEPTION_INIT(value_too_large, -12899);
BEGIN
   newrec_.rowversion := sysdate;
   newrec_.rowkey := sys_guid();
   Client_SYS.Add_To_Attr('OBJKEY', newrec_.rowkey, attr_);
   newrec_.rowtype := lu_name_;
   INSERT
      INTO trn_customer_tab
      VALUES newrec_
      RETURNING rowid INTO objid_;
   objversion_ := to_char(newrec_.rowversion,'YYYYMMDDHH24MISS');
EXCEPTION
   WHEN dup_val_on_index THEN
      DECLARE
         constraint_ VARCHAR2(4000) := Utility_SYS.Get_Constraint_From_Error_Msg(sqlerrm);
      BEGIN
         IF (constraint_ = 'TRN_CUSTOMER_RK') THEN
            Error_SYS.Fnd_Rowkey_Exist(lu_name_, newrec_.rowkey);
         ELSIF (constraint_ = 'TRN_CUSTOMER_PK') THEN
            Raise_Record_Exist___(newrec_);
         ELSE
            Raise_Constraint_Violated___(newrec_, constraint_);
         END IF;
      END;
   WHEN value_too_large THEN
      Error_SYS.Fnd_Item_Length(lu_name_, sqlerrm);
END Insert___;


-- Prepare_New___
--    Set default values for a table record.
PROCEDURE Prepare_New___ (
   newrec_ IN OUT trn_customer_tab%ROWTYPE )
IS
   attr_    VARCHAR2(32000);
   indrec_  Indicator_Rec;
BEGIN
   attr_ := Pack___(newrec_);
   Prepare_Insert___(attr_);
   Unpack___(newrec_, indrec_, attr_);
END Prepare_New___;


-- New___
--    Checks and creates a new record.
PROCEDURE New___ (
   newrec_ IN OUT trn_customer_tab%ROWTYPE )
IS
   objid_         VARCHAR2(20);
   objversion_    VARCHAR2(100);
   attr_          VARCHAR2(32000);
   indrec_        Indicator_Rec;
BEGIN
   indrec_ := Get_Indicator_Rec___(newrec_);
   Check_Insert___(newrec_, indrec_, attr_);
   Insert___(objid_, objversion_, newrec_, attr_);
END New___;


-- Check_Update___
--   Perform validations on a new record before it is updated.
PROCEDURE Check_Update___ (
   oldrec_ IN     trn_customer_tab%ROWTYPE,
   newrec_ IN OUT trn_customer_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
BEGIN
   Validate_SYS.Item_Update(lu_name_, 'COMPANY_ID', indrec_.company_id);
   Validate_SYS.Item_Update(lu_name_, 'CUSTOMER_ID', indrec_.customer_id);
   Check_Common___(oldrec_, newrec_, indrec_, attr_);
END Check_Update___;


-- Update___
--   Update a record in database with new data.
PROCEDURE Update___ (
   objid_      IN     VARCHAR2,
   oldrec_     IN     trn_customer_tab%ROWTYPE,
   newrec_     IN OUT trn_customer_tab%ROWTYPE,
   attr_       IN OUT VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   by_keys_    IN     BOOLEAN DEFAULT FALSE )
IS
   value_too_large  EXCEPTION;
   PRAGMA           EXCEPTION_INIT(value_too_large, -12899);
BEGIN
   newrec_.rowversion := sysdate;
   IF by_keys_ THEN
      UPDATE trn_customer_tab
         SET ROW = newrec_
         WHERE company_id = newrec_.company_id
         AND   customer_id = newrec_.customer_id
         AND   rowtype LIKE '%TrnExternalCustomer';
   ELSE
      UPDATE trn_customer_tab
         SET ROW = newrec_
         WHERE rowid = objid_;
   END IF;
   objversion_ := to_char(newrec_.rowversion,'YYYYMMDDHH24MISS');
EXCEPTION
   WHEN dup_val_on_index THEN
      DECLARE
         constraint_ VARCHAR2(4000) := Utility_SYS.Get_Constraint_From_Error_Msg(sqlerrm);
      BEGIN
         IF (constraint_ = 'TRN_CUSTOMER_RK') THEN
            Error_SYS.Fnd_Rowkey_Exist(Trn_External_Customer_API.lu_name_, newrec_.rowkey);
         ELSIF (constraint_ = 'TRN_CUSTOMER_PK') THEN
            Raise_Record_Exist___(newrec_);
         ELSE
            Raise_Constraint_Violated___(newrec_, constraint_);
         END IF;
      END;
   WHEN value_too_large THEN
      Error_SYS.Fnd_Item_Length(lu_name_, sqlerrm);
END Update___;


-- Modify___
--    Modifies an existing instance of the logical unit.
PROCEDURE Modify___ (
   newrec_         IN OUT trn_customer_tab%ROWTYPE,
   lock_mode_wait_ IN     BOOLEAN DEFAULT TRUE )
IS
   objid_      VARCHAR2(20);
   objversion_ VARCHAR2(100);
   attr_       VARCHAR2(32000);
   indrec_     Indicator_rec;
   oldrec_     trn_customer_tab%ROWTYPE;
BEGIN
   IF (lock_mode_wait_) THEN
      oldrec_ := Lock_By_Keys___(newrec_.company_id, newrec_.customer_id);
   ELSE
      oldrec_ := Lock_By_Keys_Nowait___(newrec_.company_id, newrec_.customer_id);
   END IF;
   indrec_ := Get_Indicator_Rec___(oldrec_, newrec_);
   Check_Update___(oldrec_, newrec_, indrec_, attr_);
   Update___(objid_, oldrec_, newrec_, attr_, objversion_, TRUE);
END Modify___;


-- Check_Delete___
--   Perform validations on a new record before it is deleted.
PROCEDURE Check_Delete___ (
   remrec_ IN trn_customer_tab%ROWTYPE )
IS
   key_ VARCHAR2(2000);
BEGIN
   key_ := remrec_.company_id||'^'||remrec_.customer_id||'^';
   Reference_SYS.Check_Restricted_Delete(lu_name_, key_);
END Check_Delete___;


-- Delete___
--   Delete a record from the database.
PROCEDURE Delete___ (
   objid_  IN VARCHAR2,
   remrec_ IN trn_customer_tab%ROWTYPE )
IS
   key_ VARCHAR2(2000);
BEGIN
   key_ := remrec_.company_id||'^'||remrec_.customer_id||'^';
   Reference_SYS.Do_Cascade_Delete(lu_name_, key_);
   IF (objid_ IS NOT NULL) THEN
      DELETE
         FROM  trn_customer_tab
         WHERE rowid = objid_;
   ELSE
      DELETE
         FROM  trn_customer_tab
         WHERE company_id = remrec_.company_id
         AND   customer_id = remrec_.customer_id
         AND   rowtype LIKE '%TrnExternalCustomer';
   END IF;
END Delete___;


-- Delete___
--   Delete a record from the database.
@Deprecated
PROCEDURE Delete___ (
   remrec_ IN trn_customer_tab%ROWTYPE )
IS
BEGIN
   Delete___(NULL, remrec_);
END Delete___;


-- Remove___
--    Removes an existing instance of the logical unit.
PROCEDURE Remove___ (
   remrec_         IN OUT trn_customer_tab%ROWTYPE,
   lock_mode_wait_ IN     BOOLEAN DEFAULT TRUE )
IS
   oldrec_     trn_customer_tab%ROWTYPE;
BEGIN
   IF (lock_mode_wait_) THEN
      oldrec_ := Lock_By_Keys___(remrec_.company_id, remrec_.customer_id);
   ELSE
      oldrec_ := Lock_By_Keys_Nowait___(remrec_.company_id, remrec_.customer_id);
   END IF;
   Check_Delete___(oldrec_);
   Delete___(NULL, oldrec_);
END Remove___;


-- Lock__
--    Client-support to lock a specific instance of the logical unit.
@UncheckedAccess
PROCEDURE Lock__ (
   info_       OUT VARCHAR2,
   objid_      IN  VARCHAR2,
   objversion_ IN  VARCHAR2 )
IS
   dummy_ trn_customer_tab%ROWTYPE;
BEGIN
   dummy_ := Lock_By_Id___(objid_, objversion_);
   info_ := Client_SYS.Get_All_Info;
END Lock__;


-- New__
--    Client-support interface to create LU instances.
--       action_ = 'PREPARE'
--          Default values and handle of information to client.
--          The default values are set in procedure Prepare_Insert___.
--       action_ = 'CHECK'
--          Check all attributes before creating new object and handle of
--          information to client. The attribute list is unpacked, checked
--          and prepared (defaults) in procedures Unpack___ and Check_Insert___.
--       action_ = 'DO'
--          Creation of new instances of the logical unit and handle of
--          information to client. The attribute list is unpacked, checked
--          and prepared (defaults) in procedures Unpack___ and Check_Insert___
--          before calling procedure Insert___.
PROCEDURE New__ (
   info_       OUT    VARCHAR2,
   objid_      OUT    VARCHAR2,
   objversion_ OUT    VARCHAR2,
   attr_       IN OUT VARCHAR2,
   action_     IN     VARCHAR2 )
IS
   newrec_   trn_customer_tab%ROWTYPE;
   indrec_   Indicator_Rec;
BEGIN
   IF (action_ = 'PREPARE') THEN
      Prepare_Insert___(attr_);
   ELSIF (action_ = 'CHECK') THEN
      Unpack___(newrec_, indrec_, attr_);
      Check_Insert___(newrec_, indrec_, attr_);
   ELSIF (action_ = 'DO') THEN
      Unpack___(newrec_, indrec_, attr_);
      Check_Insert___(newrec_, indrec_, attr_);
      Insert___(objid_, objversion_, newrec_, attr_);
   END IF;
   info_ := Client_SYS.Get_All_Info;
END New__;


-- Modify__
--    Client-support interface to modify attributes for LU instances.
--       action_ = 'CHECK'
--          Check all attributes before modifying an existing object and
--          handle of information to client. The attribute list is unpacked,
--          checked and prepared(defaults) in procedures Unpack___ and Check_Update___.
--       action_ = 'DO'
--          Modification of an existing instance of the logical unit. The
--          procedure unpacks the attributes, checks all values before
--          procedure Update___ is called.
PROCEDURE Modify__ (
   info_       OUT    VARCHAR2,
   objid_      IN     VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   attr_       IN OUT VARCHAR2,
   action_     IN     VARCHAR2 )
IS
   oldrec_   trn_customer_tab%ROWTYPE;
   newrec_   trn_customer_tab%ROWTYPE;
   indrec_   Indicator_Rec;
BEGIN
   IF (action_ = 'CHECK') THEN
      oldrec_ := Get_Object_By_Id___(objid_);
      newrec_ := oldrec_;
      Unpack___(newrec_, indrec_, attr_);
      Check_Update___(oldrec_, newrec_, indrec_, attr_);
   ELSIF (action_ = 'DO') THEN
      oldrec_ := Lock_By_Id___(objid_, objversion_);
      newrec_ := oldrec_;
      Unpack___(newrec_, indrec_, attr_);
      Check_Update___(oldrec_, newrec_, indrec_, attr_);
      Update___(objid_, oldrec_, newrec_, attr_, objversion_);
   END IF;
   info_ := Client_SYS.Get_All_Info;
END Modify__;


-- Remove__
--    Client-support interface to remove LU instances.
--       action_ = 'CHECK'
--          Check whether a specific LU-instance may be removed or not.
--          The procedure fetches the complete record by calling procedure
--          Get_Object_By_Id___. Then the check is made by calling procedure
--          Check_Delete___.
--       action_ = 'DO'
--          Remove an existing instance of the logical unit. The procedure
--          fetches the complete LU-record, checks for a delete and then
--          deletes the record by calling procedure Delete___.
PROCEDURE Remove__ (
   info_       OUT VARCHAR2,
   objid_      IN  VARCHAR2,
   objversion_ IN  VARCHAR2,
   action_     IN  VARCHAR2 )
IS
   remrec_ trn_customer_tab%ROWTYPE;
BEGIN
   IF (action_ = 'CHECK') THEN
      remrec_ := Get_Object_By_Id___(objid_);
      Check_Delete___(remrec_);
   ELSIF (action_ = 'DO') THEN
      remrec_ := Lock_By_Id___(objid_, objversion_);
      Check_Delete___(remrec_);
      Delete___(objid_, remrec_);
   END IF;
   info_ := Client_SYS.Get_All_Info;
END Remove__;


-- Get_Key_By_Rowkey
--   Returns a table record with only keys (other attributes are NULL) based on a rowkey.
@UncheckedAccess
FUNCTION Get_Key_By_Rowkey (
   rowkey_ IN VARCHAR2 ) RETURN trn_customer_tab%ROWTYPE
IS
   rec_ trn_customer_tab%ROWTYPE;
BEGIN
   IF (rowkey_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT company_id, customer_id
      INTO  rec_.company_id, rec_.customer_id
      FROM  trn_customer_tab
      WHERE rowkey = rowkey_;
   RETURN rec_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN rec_;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(rec_.company_id, rec_.customer_id, 'Get_Key_By_Rowkey');
END Get_Key_By_Rowkey;


-- Exist
--   Checks if given pointer (e.g. primary key) to an instance of this
--   logical unit exists. If not an exception will be raised.
@UncheckedAccess
PROCEDURE Exist (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER )
IS
BEGIN
   IF (NOT Check_Exist___(company_id_, customer_id_)) THEN
      Raise_Record_Not_Exist___(company_id_, customer_id_);
   END IF;
END Exist;


-- Exists
--   Same check as Exist, but returns a BOOLEAN value instead of exception.
@UncheckedAccess
FUNCTION Exists (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN BOOLEAN
IS
BEGIN
   RETURN Check_Exist___(company_id_, customer_id_);
END Exists;

-- Rowkey_Exist
--   Checks whether the rowkey exists
--   If not an exception will be raised.
@UncheckedAccess
PROCEDURE Rowkey_Exist (
   rowkey_ IN VARCHAR2 )
IS
   company_id_ trn_customer_tab.company_id%TYPE;
   customer_id_ trn_customer_tab.customer_id%TYPE;
BEGIN
   IF (rowkey_ IS NULL) THEN
      RAISE no_data_found;
   END IF;
   SELECT company_id, customer_id
   INTO  company_id_, customer_id_
   FROM  trn_customer_tab
   WHERE rowkey = rowkey_;
EXCEPTION
   WHEN no_data_found THEN
      Raise_Record_Not_Exist___(company_id_, customer_id_);
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Rowkey_Exist___');
END Rowkey_Exist;


-- Get_Name
--   Fetches the Name attribute for a record.
@UncheckedAccess
FUNCTION Get_Name (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN VARCHAR2
IS
   temp_ trn_customer_tab.name%TYPE;
BEGIN
   IF (company_id_ IS NULL OR customer_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT name
      INTO  temp_
      FROM  trn_customer_tab
      WHERE company_id = company_id_
      AND   customer_id = customer_id_
      AND   rowtype LIKE '%TrnExternalCustomer';
   RETURN temp_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Get_Name');
END Get_Name;


-- Get_Credit_Limit
--   Fetches the CreditLimit attribute for a record.
@UncheckedAccess
FUNCTION Get_Credit_Limit (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN NUMBER
IS
   temp_ trn_customer_tab.credit_limit%TYPE;
BEGIN
   IF (company_id_ IS NULL OR customer_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT credit_limit
      INTO  temp_
      FROM  trn_customer_tab
      WHERE company_id = company_id_
      AND   customer_id = customer_id_
      AND   rowtype LIKE '%TrnExternalCustomer';
   RETURN temp_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Get_Credit_Limit');
END Get_Credit_Limit;


-- Get_Discount
--   Fetches the Discount attribute for a record.
@UncheckedAccess
FUNCTION Get_Discount (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN NUMBER
IS
   temp_ trn_customer_tab.discount%TYPE;
BEGIN
   IF (company_id_ IS NULL OR customer_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT discount
      INTO  temp_
      FROM  trn_customer_tab
      WHERE company_id = company_id_
      AND   customer_id = customer_id_
      AND   rowtype LIKE '%TrnExternalCustomer';
   RETURN temp_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Get_Discount');
END Get_Discount;


-- Get_Active
--   Fetches the Active attribute for a record.
@UncheckedAccess
FUNCTION Get_Active (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN VARCHAR2
IS
   temp_ trn_customer_tab.active%TYPE;
BEGIN
   IF (company_id_ IS NULL OR customer_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT active
      INTO  temp_
      FROM  trn_customer_tab
      WHERE company_id = company_id_
      AND   customer_id = customer_id_
      AND   rowtype LIKE '%TrnExternalCustomer';
   RETURN Fnd_Boolean_API.Decode(temp_);
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Get_Active');
END Get_Active;


-- Get_Active_Db
--   Fetches the DB value of Active attribute for a record.
@UncheckedAccess
FUNCTION Get_Active_Db (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN trn_customer_tab.active%TYPE
IS
   temp_ trn_customer_tab.active%TYPE;
BEGIN
   IF (company_id_ IS NULL OR customer_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT active
      INTO  temp_
      FROM  trn_customer_tab
      WHERE company_id = company_id_
      AND   customer_id = customer_id_
      AND   rowtype LIKE '%TrnExternalCustomer';
   RETURN temp_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Get_Active_Db');
END Get_Active_Db;


-- Get_Branch_Id
--   Fetches the BranchId attribute for a record.
@UncheckedAccess
FUNCTION Get_Branch_Id (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN VARCHAR2
IS
   temp_ trn_customer_tab.branch_id%TYPE;
BEGIN
   IF (company_id_ IS NULL OR customer_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT branch_id
      INTO  temp_
      FROM  trn_customer_tab
      WHERE company_id = company_id_
      AND   customer_id = customer_id_
      AND   rowtype LIKE '%TrnExternalCustomer';
   RETURN temp_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Get_Branch_Id');
END Get_Branch_Id;


-- Get_Preferred_Inventory_Id
--   Fetches the PreferredInventoryId attribute for a record.
@UncheckedAccess
FUNCTION Get_Preferred_Inventory_Id (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN NUMBER
IS
   temp_ trn_customer_tab.preferred_inventory_id%TYPE;
BEGIN
   IF (company_id_ IS NULL OR customer_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT preferred_inventory_id
      INTO  temp_
      FROM  trn_customer_tab
      WHERE company_id = company_id_
      AND   customer_id = customer_id_
      AND   rowtype LIKE '%TrnExternalCustomer';
   RETURN temp_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Get_Preferred_Inventory_Id');
END Get_Preferred_Inventory_Id;

-- Get_By_Rowkey
--   Fetches a record containing the public attributes by rowkey inparameter.
@UncheckedAccess
FUNCTION Get_By_Rowkey (
   rowkey_ IN VARCHAR2 ) RETURN Public_Rec
IS
   rowrec_ trn_customer_tab%ROWTYPE;
BEGIN
   rowrec_ := Get_Key_By_Rowkey(rowkey_);
   RETURN Get(rowrec_.company_id, rowrec_.customer_id);
END Get_By_Rowkey;

-- Get
--   Fetches a record containing the public attributes.
@UncheckedAccess
FUNCTION Get (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN Public_Rec
IS
   temp_ Public_Rec;
BEGIN
   IF (company_id_ IS NULL OR customer_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT company_id, customer_id, rowid, rowversion, rowkey, rowtype,
          name, 
          credit_limit, 
          discount, 
          active, 
          branch_id, 
          preferred_inventory_id
      INTO  temp_
      FROM  trn_customer_tab
      WHERE company_id = company_id_
      AND   customer_id = customer_id_
      AND   rowtype LIKE '%TrnExternalCustomer';
   RETURN temp_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Get');
END Get;


-- Get_Objkey
--   Fetches the objkey attribute for a record.
@UncheckedAccess
FUNCTION Get_Objkey (
   company_id_ IN VARCHAR2,
   customer_id_ IN NUMBER ) RETURN VARCHAR2
IS
   rowkey_ trn_customer_tab.rowkey%TYPE;
BEGIN
   IF (company_id_ IS NULL OR customer_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT rowkey
      INTO  rowkey_
      FROM  trn_customer_tab
      WHERE company_id = company_id_
      AND   customer_id = customer_id_
      AND   rowtype LIKE '%TrnExternalCustomer';
   RETURN rowkey_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, customer_id_, 'Get_Objkey');
END Get_Objkey;



-------------------- COMPLEX STRUCTURE METHODS ------------------------------------

-------------------- FOUNDATION1 METHODS ------------------------------------


-- Init
--   Framework method that initializes this package.
@UncheckedAccess
PROCEDURE Init
IS
BEGIN
   NULL;
END Init;