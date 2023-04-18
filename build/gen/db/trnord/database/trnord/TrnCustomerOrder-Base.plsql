-----------------------------------------------------------------------------
--
--  Logical unit: TrnCustomerOrder
--  Component:    TRNORD
--
--  Template:     3.0
--  Built by:     IFS Developer Studio (unit-test)
--
--  NOTE! Do not edit!! This file is completely generated and will be
--        overwritten next time the corresponding model is saved.
-----------------------------------------------------------------------------

layer Base;

-------------------- PUBLIC DECLARATIONS ------------------------------------

--TYPE Primary_Key_Rec IS RECORD
--  (company_id                     TRN_CUSTOMER_ORDER_TAB.company_id%TYPE,
--   branch_id                      TRN_CUSTOMER_ORDER_TAB.branch_id%TYPE,
--   order_id                       TRN_CUSTOMER_ORDER_TAB.order_id%TYPE);

TYPE Public_Rec IS RECORD
  (company_id                     TRN_CUSTOMER_ORDER_TAB.company_id%TYPE,
   branch_id                      TRN_CUSTOMER_ORDER_TAB.branch_id%TYPE,
   order_id                       TRN_CUSTOMER_ORDER_TAB.order_id%TYPE,
   "rowid"                        rowid,
   rowversion                     TRN_CUSTOMER_ORDER_TAB.rowversion%TYPE,
   rowkey                         TRN_CUSTOMER_ORDER_TAB.rowkey%TYPE,
   rowstate                       TRN_CUSTOMER_ORDER_TAB.rowstate%TYPE,
   customer_id                    TRN_CUSTOMER_ORDER_TAB.customer_id%TYPE,
   delivery_type                  TRN_CUSTOMER_ORDER_TAB.delivery_type%TYPE);




-------------------- PRIVATE DECLARATIONS -----------------------------------

TYPE Indicator_Rec IS RECORD
  (company_id                     BOOLEAN := FALSE,
   branch_id                      BOOLEAN := FALSE,
   order_id                       BOOLEAN := FALSE,
   order_date                     BOOLEAN := FALSE,
   delivery_date                  BOOLEAN := FALSE,
   discount                       BOOLEAN := FALSE,
   comments                       BOOLEAN := FALSE,
   customer_id                    BOOLEAN := FALSE,
   delivery_type                  BOOLEAN := FALSE);

-------------------- BASE METHODS -------------------------------------------

-- Key_Message___
--    Returns an error message containing the keys.
FUNCTION Key_Message___ (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN VARCHAR2 
IS
   msg_ VARCHAR2(4000) := Message_SYS.Construct('ERROR_KEY');
BEGIN
   Message_SYS.Add_Attribute(msg_, 'COMPANY_ID', company_id_);
   Message_SYS.Add_Attribute(msg_, 'BRANCH_ID', branch_id_);
   Message_SYS.Add_Attribute(msg_, 'ORDER_ID', order_id_);
   RETURN msg_;
END Key_Message___;

-- Formatted_Key___
--    Returns an error string containing the keys.
FUNCTION Formatted_Key___ (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN VARCHAR2
IS
   formatted_key_ VARCHAR2(4000) := Language_SYS.Translate_Item_Prompt_(lu_name_, 'COMPANY_ID', Fnd_Session_API.Get_Language) || ': ' || company_id_ || ', ' ||
                                    Language_SYS.Translate_Item_Prompt_(lu_name_, 'BRANCH_ID', Fnd_Session_API.Get_Language) || ': ' || branch_id_ || ', ' ||
                                    Language_SYS.Translate_Item_Prompt_(lu_name_, 'ORDER_ID', Fnd_Session_API.Get_Language) || ': ' || order_id_;
BEGIN
   RETURN formatted_key_;
END Formatted_Key___;

-- Raise_Too_Many_Rows___
--    Raises error for: More then one row found for a single key.
PROCEDURE Raise_Too_Many_Rows___ (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER,
   methodname_ IN VARCHAR2 )
IS
BEGIN
   Error_SYS.Set_Key_Values(Key_Message___(company_id_, branch_id_, order_id_),
                            Formatted_Key___(company_id_, branch_id_, order_id_));
   Error_SYS.Fnd_Too_Many_Rows(Trn_Customer_Order_API.lu_name_, NULL, methodname_);
END Raise_Too_Many_Rows___;


-- Raise_Record_Not_Exist___
--    Raises error for: No data found for given key.
PROCEDURE Raise_Record_Not_Exist___ (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER )
IS
BEGIN
   Error_SYS.Set_Key_Values(Key_Message___(company_id_, branch_id_, order_id_),
                            Formatted_Key___(company_id_, branch_id_, order_id_));
   Error_SYS.Fnd_Record_Not_Exist(Trn_Customer_Order_API.lu_name_);
END Raise_Record_Not_Exist___;


-- Raise_Record_Exist___
--    Raises error for: Data with given key value already exist.
PROCEDURE Raise_Record_Exist___ (
   rec_ IN trn_customer_order_tab%ROWTYPE )
IS
BEGIN
   Error_SYS.Set_Key_Values(Key_Message___(rec_.company_id, rec_.branch_id, rec_.order_id),
                            Formatted_Key___(rec_.company_id, rec_.branch_id, rec_.order_id));
   Error_SYS.Fnd_Record_Exist(Trn_Customer_Order_API.lu_name_);
END Raise_Record_Exist___;

-- Raise_Constraint_Violated___
--    Raises error for: Data with given value for constraint that already exist.
--    constraint_ contains the violated constraint to be used when overriding the method.
PROCEDURE Raise_Constraint_Violated___ (
   rec_ IN trn_customer_order_tab%ROWTYPE,
   constraint_ IN VARCHAR2 )
IS
BEGIN
   Error_SYS.Fnd_Record_Exist(Trn_Customer_Order_API.lu_name_);
END Raise_Constraint_Violated___;


-- Raise_Item_Format___
--    Raises error for: Data value format is incorrect.
PROCEDURE Raise_Item_Format___ (
   name_ IN VARCHAR2,
   value_ IN VARCHAR2 )
IS
BEGIN
   Error_SYS.Fnd_Item_Format(Trn_Customer_Order_API.lu_name_, name_, value_);
END Raise_Item_Format___;

-- Raise_Record_Modified___
--    Raises error for: The database row is newer then the current.
PROCEDURE Raise_Record_Modified___ (
   rec_ IN trn_customer_order_tab%ROWTYPE )
IS
BEGIN
   Error_SYS.Set_Key_Values(Key_Message___(rec_.company_id, rec_.branch_id, rec_.order_id),
                            Formatted_Key___(rec_.company_id, rec_.branch_id, rec_.order_id));
   Error_SYS.Fnd_Record_Modified(Trn_Customer_Order_API.lu_name_);
END Raise_Record_Modified___;


-- Raise_Record_Locked___
--    Raises error for: The database row is already locked.
PROCEDURE Raise_Record_Locked___ (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER )
IS
BEGIN
   Error_SYS.Set_Key_Values(Key_Message___(company_id_, branch_id_, order_id_),
                            Formatted_Key___(company_id_, branch_id_, order_id_));
   Error_SYS.Fnd_Record_Locked(Trn_Customer_Order_API.lu_name_);
END Raise_Record_Locked___;


-- Raise_Record_Removed___
--    Raises error for: The database row is no longer present.
PROCEDURE Raise_Record_Removed___ (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER )
IS
BEGIN
   Error_SYS.Set_Key_Values(Key_Message___(company_id_, branch_id_, order_id_),
                            Formatted_Key___(company_id_, branch_id_, order_id_));
   Error_SYS.Fnd_Record_Removed(Trn_Customer_Order_API.lu_name_);
END Raise_Record_Removed___;


-- Lock_By_Id___
--    Locks a database row based on the objid and objversion.
FUNCTION Lock_By_Id___ (
   objid_      IN VARCHAR2,
   objversion_ IN VARCHAR2 ) RETURN trn_customer_order_tab%ROWTYPE
IS
   row_locked  EXCEPTION;
   PRAGMA      EXCEPTION_INIT(row_locked, -0054);
   rec_        trn_customer_order_tab%ROWTYPE;
BEGIN
   SELECT *
      INTO  rec_
      FROM  trn_customer_order_tab
      WHERE rowid = objid_
      AND    to_char(rowversion,'YYYYMMDDHH24MISS') = objversion_
      FOR UPDATE NOWAIT;
   RETURN rec_;
EXCEPTION
   WHEN row_locked THEN
      Error_SYS.Fnd_Record_Locked(lu_name_);
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(NULL, NULL, NULL, 'Lock_By_Id___');
   WHEN no_data_found THEN
      BEGIN
         SELECT *
            INTO  rec_
            FROM  trn_customer_order_tab
            WHERE rowid = objid_;
         Raise_Record_Modified___(rec_);
      EXCEPTION
         WHEN no_data_found THEN
            Error_SYS.Fnd_Record_Removed(lu_name_);
         WHEN too_many_rows THEN
            Raise_Too_Many_Rows___(NULL, NULL, NULL, 'Lock_By_Id___');
      END;
END Lock_By_Id___;


-- Lock_By_Keys___
--    Locks a database row based on the primary key values.
--    Waits until record released if locked by another session.
FUNCTION Lock_By_Keys___ (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER) RETURN trn_customer_order_tab%ROWTYPE
IS
   rec_        trn_customer_order_tab%ROWTYPE;
BEGIN
   BEGIN
      SELECT *
         INTO  rec_
         FROM  trn_customer_order_tab
         WHERE company_id = company_id_
         AND   branch_id = branch_id_
         AND   order_id = order_id_
         FOR UPDATE;
      RETURN rec_;
   EXCEPTION
      WHEN no_data_found THEN
         Raise_Record_Removed___(company_id_, branch_id_, order_id_);
      WHEN too_many_rows THEN
         Raise_Too_Many_Rows___(company_id_, branch_id_, order_id_, 'Lock_By_Keys___');
   END;
END Lock_By_Keys___;


-- Lock_By_Keys_Nowait___
--    Locks a database row based on the primary key values.
--    Raises exception if row already locked.
FUNCTION Lock_By_Keys_Nowait___ (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER) RETURN trn_customer_order_tab%ROWTYPE
IS
   row_locked  EXCEPTION;
   PRAGMA      EXCEPTION_INIT(row_locked, -0054);
   rec_        trn_customer_order_tab%ROWTYPE;
BEGIN
   BEGIN
      SELECT *
         INTO  rec_
         FROM  trn_customer_order_tab
         WHERE company_id = company_id_
         AND   branch_id = branch_id_
         AND   order_id = order_id_
         FOR UPDATE NOWAIT;
      RETURN rec_;
   EXCEPTION
      WHEN row_locked THEN
         Raise_Record_Locked___(company_id_, branch_id_, order_id_);
      WHEN too_many_rows THEN
         Raise_Too_Many_Rows___(company_id_, branch_id_, order_id_, 'Lock_By_Keys___');
      WHEN no_data_found THEN
         Raise_Record_Removed___(company_id_, branch_id_, order_id_);
   END;
END Lock_By_Keys_Nowait___;


-- Get_Object_By_Id___
--    Fetched a database row based on given the objid.
FUNCTION Get_Object_By_Id___ (
   objid_ IN VARCHAR2 ) RETURN trn_customer_order_tab%ROWTYPE
IS
   lu_rec_ trn_customer_order_tab%ROWTYPE;
BEGIN
   SELECT *
      INTO  lu_rec_
      FROM  trn_customer_order_tab
      WHERE rowid = objid_;
   RETURN lu_rec_;
EXCEPTION
   WHEN no_data_found THEN
      Error_SYS.Fnd_Record_Removed(lu_name_);
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(NULL, NULL, NULL, 'Get_Object_By_Id___');
END Get_Object_By_Id___;


-- Get_Object_By_Keys___
--    Fetched a database row based on given the primary key values.
@UncheckedAccess
FUNCTION Get_Object_By_Keys___ (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN trn_customer_order_tab%ROWTYPE
IS
   lu_rec_ trn_customer_order_tab%ROWTYPE;
BEGIN
   SELECT *
      INTO  lu_rec_
      FROM  trn_customer_order_tab
      WHERE company_id = company_id_
      AND   branch_id = branch_id_
      AND   order_id = order_id_;
   RETURN lu_rec_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN lu_rec_;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, branch_id_, order_id_, 'Get_Object_By_Keys___');
END Get_Object_By_Keys___;


-- Check_Exist___
--    Checks if a database row is already stored based on the primary key values.
@UncheckedAccess
FUNCTION Check_Exist___ (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN BOOLEAN
IS
   dummy_ NUMBER;
BEGIN
   IF (company_id_ IS NULL OR branch_id_ IS NULL OR order_id_ IS NULL) THEN
      RETURN FALSE;
   END IF;
   SELECT 1
      INTO  dummy_
      FROM  trn_customer_order_tab
      WHERE company_id = company_id_
      AND   branch_id = branch_id_
      AND   order_id = order_id_;
   RETURN TRUE;
EXCEPTION
   WHEN no_data_found THEN
      RETURN FALSE;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, branch_id_, order_id_, 'Check_Exist___');
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
      FROM  trn_customer_order_tab
      WHERE rowid = objid_;
EXCEPTION
   WHEN no_data_found THEN
      objversion_ := NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(NULL, NULL, NULL, 'Get_Version_By_Id___');
END Get_Version_By_Id___;


-- Get_Version_By_Keys___
--    Fetched the objversion for a database row based on the primary key.
PROCEDURE Get_Id_Version_By_Keys___ (
   objid_      IN OUT VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER )
IS
BEGIN
   SELECT rowid, to_char(rowversion,'YYYYMMDDHH24MISS')
      INTO  objid_, objversion_
      FROM  trn_customer_order_tab
      WHERE company_id = company_id_
      AND   branch_id = branch_id_
      AND   order_id = order_id_;
EXCEPTION
   WHEN no_data_found THEN
      objid_      := NULL;
      objversion_ := NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, branch_id_, order_id_, 'Get_Id_Version_By_Keys___');
END Get_Id_Version_By_Keys___;


-- Unpack___
--   Reads an attribute string and unpacks its contents into a record.
PROCEDURE Unpack___ (
   newrec_   IN OUT trn_customer_order_tab%ROWTYPE,
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
      WHEN ('BRANCH_ID') THEN
         newrec_.branch_id := value_;
         indrec_.branch_id := TRUE;
      WHEN ('ORDER_ID') THEN
         newrec_.order_id := Client_SYS.Attr_Value_To_Number(value_);
         indrec_.order_id := TRUE;
      WHEN ('ORDER_DATE') THEN
         newrec_.order_date := Client_SYS.Attr_Value_To_Date(value_);
         indrec_.order_date := TRUE;
      WHEN ('DELIVERY_DATE') THEN
         newrec_.delivery_date := Client_SYS.Attr_Value_To_Date(value_);
         indrec_.delivery_date := TRUE;
      WHEN ('DISCOUNT') THEN
         newrec_.discount := Client_SYS.Attr_Value_To_Number(value_);
         indrec_.discount := TRUE;
      WHEN ('COMMENTS') THEN
         newrec_.comments := value_;
         indrec_.comments := TRUE;
      WHEN ('CUSTOMER_ID') THEN
         newrec_.customer_id := Client_SYS.Attr_Value_To_Number(value_);
         indrec_.customer_id := TRUE;
      WHEN ('DELIVERY_TYPE') THEN
         newrec_.delivery_type := Trn_Order_Del_Type_API.Encode(value_);
         IF (value_ IS NOT NULL AND newrec_.delivery_type IS NULL) THEN
            RAISE value_error;
         END IF;
         indrec_.delivery_type := TRUE;
      WHEN ('DELIVERY_TYPE_DB') THEN
         newrec_.delivery_type := value_;
         indrec_.delivery_type := TRUE;
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
   rec_ IN trn_customer_order_tab%ROWTYPE ) RETURN VARCHAR2
IS
   attr_ VARCHAR2(32000);
BEGIN
   Client_SYS.Clear_Attr(attr_);
   IF (rec_.company_id IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('COMPANY_ID', rec_.company_id, attr_);
   END IF;
   IF (rec_.branch_id IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('BRANCH_ID', rec_.branch_id, attr_);
   END IF;
   IF (rec_.order_id IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('ORDER_ID', rec_.order_id, attr_);
   END IF;
   IF (rec_.order_date IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('ORDER_DATE', rec_.order_date, attr_);
   END IF;
   IF (rec_.delivery_date IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('DELIVERY_DATE', rec_.delivery_date, attr_);
   END IF;
   IF (rec_.discount IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('DISCOUNT', rec_.discount, attr_);
   END IF;
   IF (rec_.comments IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('COMMENTS', rec_.comments, attr_);
   END IF;
   IF (rec_.customer_id IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('CUSTOMER_ID', rec_.customer_id, attr_);
   END IF;
   IF (rec_.delivery_type IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('DELIVERY_TYPE', Trn_Order_Del_Type_API.Decode(rec_.delivery_type), attr_);
      Client_SYS.Add_To_Attr('DELIVERY_TYPE_DB', rec_.delivery_type, attr_);
   END IF;
   RETURN attr_;
END Pack___;


FUNCTION Pack___ (
   rec_ IN trn_customer_order_tab%ROWTYPE,
   indrec_ IN Indicator_Rec ) RETURN VARCHAR2
IS
   attr_ VARCHAR2(32000);
BEGIN
   Client_SYS.Clear_Attr(attr_);
   IF (indrec_.company_id) THEN
      Client_SYS.Add_To_Attr('COMPANY_ID', rec_.company_id, attr_);
   END IF;
   IF (indrec_.branch_id) THEN
      Client_SYS.Add_To_Attr('BRANCH_ID', rec_.branch_id, attr_);
   END IF;
   IF (indrec_.order_id) THEN
      Client_SYS.Add_To_Attr('ORDER_ID', rec_.order_id, attr_);
   END IF;
   IF (indrec_.order_date) THEN
      Client_SYS.Add_To_Attr('ORDER_DATE', rec_.order_date, attr_);
   END IF;
   IF (indrec_.delivery_date) THEN
      Client_SYS.Add_To_Attr('DELIVERY_DATE', rec_.delivery_date, attr_);
   END IF;
   IF (indrec_.discount) THEN
      Client_SYS.Add_To_Attr('DISCOUNT', rec_.discount, attr_);
   END IF;
   IF (indrec_.comments) THEN
      Client_SYS.Add_To_Attr('COMMENTS', rec_.comments, attr_);
   END IF;
   IF (indrec_.customer_id) THEN
      Client_SYS.Add_To_Attr('CUSTOMER_ID', rec_.customer_id, attr_);
   END IF;
   IF (indrec_.delivery_type) THEN
      Client_SYS.Add_To_Attr('DELIVERY_TYPE', Trn_Order_Del_Type_API.Decode(rec_.delivery_type), attr_);
      Client_SYS.Add_To_Attr('DELIVERY_TYPE_DB', rec_.delivery_type, attr_);
   END IF;
   RETURN attr_;
END Pack___;



-- Pack_Table___
--   Reads a record and packs its contents into an attribute string.
--   Similar to Pack___ but just uses table column names and DB values
FUNCTION Pack_Table___ (
   rec_ IN trn_customer_order_tab%ROWTYPE ) RETURN VARCHAR2
IS
   attr_ VARCHAR2(32000);
BEGIN
   Client_SYS.Clear_Attr(attr_);
   Client_SYS.Add_To_Attr('COMPANY_ID', rec_.company_id, attr_);
   Client_SYS.Add_To_Attr('BRANCH_ID', rec_.branch_id, attr_);
   Client_SYS.Add_To_Attr('ORDER_ID', rec_.order_id, attr_);
   Client_SYS.Add_To_Attr('ORDER_DATE', rec_.order_date, attr_);
   Client_SYS.Add_To_Attr('DELIVERY_DATE', rec_.delivery_date, attr_);
   Client_SYS.Add_To_Attr('DISCOUNT', rec_.discount, attr_);
   Client_SYS.Add_To_Attr('COMMENTS', rec_.comments, attr_);
   Client_SYS.Add_To_Attr('CUSTOMER_ID', rec_.customer_id, attr_);
   Client_SYS.Add_To_Attr('DELIVERY_TYPE', rec_.delivery_type, attr_);
   Client_SYS.Add_To_Attr('ROWKEY', rec_.rowkey, attr_);
   Client_SYS.Add_To_Attr('ROWSTATE', rec_.rowstate, attr_);
   RETURN attr_;
END Pack_Table___;



-- Public_To_Table___
--   Reads values in the public_rec record and returns them in a table rowtype record.
FUNCTION Public_To_Table___ (
   public_ IN Public_Rec ) RETURN trn_customer_order_tab%ROWTYPE
IS
   rec_ trn_customer_order_tab%ROWTYPE;
BEGIN
   rec_.rowversion                     := public_.rowversion;
   rec_.rowkey                         := public_.rowkey;
   rec_.rowstate                       := public_.rowstate;
   rec_.company_id                     := public_.company_id;
   rec_.branch_id                      := public_.branch_id;
   rec_.order_id                       := public_.order_id;
   rec_.customer_id                    := public_.customer_id;
   rec_.delivery_type                  := public_.delivery_type;
   RETURN rec_;
END Public_To_Table___;



-- Table_To_Public___
--   Reads values in the table rowtype record and returns them in a public_rec record.
FUNCTION Table_To_Public___ (
   rec_ IN trn_customer_order_tab%ROWTYPE ) RETURN Public_Rec
IS
   public_ Public_Rec;
BEGIN
   public_.rowversion                     := rec_.rowversion;
   public_.rowkey                         := rec_.rowkey;
   public_.rowstate                       := rec_.rowstate;
   public_.company_id                     := rec_.company_id;
   public_.branch_id                      := rec_.branch_id;
   public_.order_id                       := rec_.order_id;
   public_.customer_id                    := rec_.customer_id;
   public_.delivery_type                  := rec_.delivery_type;
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
   rec_ IN trn_customer_order_tab%ROWTYPE ) RETURN Indicator_Rec
IS
   indrec_ Indicator_Rec;
BEGIN
   indrec_.company_id := rec_.company_id IS NOT NULL;
   indrec_.branch_id := rec_.branch_id IS NOT NULL;
   indrec_.order_id := rec_.order_id IS NOT NULL;
   indrec_.order_date := rec_.order_date IS NOT NULL;
   indrec_.delivery_date := rec_.delivery_date IS NOT NULL;
   indrec_.discount := rec_.discount IS NOT NULL;
   indrec_.comments := rec_.comments IS NOT NULL;
   indrec_.customer_id := rec_.customer_id IS NOT NULL;
   indrec_.delivery_type := rec_.delivery_type IS NOT NULL;
   RETURN indrec_;
END Get_Indicator_Rec___;


-- Get_Indicator_Rec___
--   Returns an Indicator_Rec that reflects the difference between two table records.
FUNCTION Get_Indicator_Rec___ (
   oldrec_ IN trn_customer_order_tab%ROWTYPE,
   newrec_ IN trn_customer_order_tab%ROWTYPE ) RETURN Indicator_Rec
IS
   indrec_ Indicator_Rec;
BEGIN
   indrec_.company_id := Validate_SYS.Is_Changed(oldrec_.company_id, newrec_.company_id);
   indrec_.branch_id := Validate_SYS.Is_Changed(oldrec_.branch_id, newrec_.branch_id);
   indrec_.order_id := Validate_SYS.Is_Changed(oldrec_.order_id, newrec_.order_id);
   indrec_.order_date := Validate_SYS.Is_Changed(oldrec_.order_date, newrec_.order_date);
   indrec_.delivery_date := Validate_SYS.Is_Changed(oldrec_.delivery_date, newrec_.delivery_date);
   indrec_.discount := Validate_SYS.Is_Changed(oldrec_.discount, newrec_.discount);
   indrec_.comments := Validate_SYS.Is_Changed(oldrec_.comments, newrec_.comments);
   indrec_.customer_id := Validate_SYS.Is_Changed(oldrec_.customer_id, newrec_.customer_id);
   indrec_.delivery_type := Validate_SYS.Is_Changed(oldrec_.delivery_type, newrec_.delivery_type);
   RETURN indrec_;
END Get_Indicator_Rec___;


-- Check_Common___
--   Perform validations on a record, that should be done for both insert and delete.
PROCEDURE Check_Common___ (
   oldrec_ IN     trn_customer_order_tab%ROWTYPE,
   newrec_ IN OUT trn_customer_order_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
BEGIN
   IF (newrec_.delivery_type IS NOT NULL)
   AND (indrec_.delivery_type)
   AND (Validate_SYS.Is_Changed(oldrec_.delivery_type, newrec_.delivery_type)) THEN
      Trn_Order_Del_Type_API.Exist_Db(newrec_.delivery_type);
   END IF;
   IF (newrec_.company_id IS NOT NULL AND newrec_.branch_id IS NOT NULL)
   AND (indrec_.company_id OR indrec_.branch_id)
   AND (Validate_SYS.Is_Changed(oldrec_.company_id, newrec_.company_id)
     OR Validate_SYS.Is_Changed(oldrec_.branch_id, newrec_.branch_id)) THEN
      Trn_Branch_API.Exist(newrec_.company_id, newrec_.branch_id);
   END IF;
   IF (newrec_.company_id IS NOT NULL AND newrec_.customer_id IS NOT NULL)
   AND (indrec_.company_id OR indrec_.customer_id)
   AND (Validate_SYS.Is_Changed(oldrec_.company_id, newrec_.company_id)
     OR Validate_SYS.Is_Changed(oldrec_.customer_id, newrec_.customer_id)) THEN
      Trn_Customer_API.Exist(newrec_.company_id, newrec_.customer_id);
   END IF;
   Error_SYS.Check_Not_Null(lu_name_, 'COMPANY_ID', newrec_.company_id);
   Error_SYS.Check_Not_Null(lu_name_, 'BRANCH_ID', newrec_.branch_id);
   Error_SYS.Check_Not_Null(lu_name_, 'ORDER_ID', newrec_.order_id);
   Error_SYS.Check_Not_Null(lu_name_, 'ORDER_DATE', newrec_.order_date);
   Error_SYS.Check_Not_Null(lu_name_, 'DELIVERY_DATE', newrec_.delivery_date);
   Error_SYS.Check_Not_Null(lu_name_, 'DISCOUNT', newrec_.discount);
   Error_SYS.Check_Not_Null(lu_name_, 'CUSTOMER_ID', newrec_.customer_id);
   Error_SYS.Check_Not_Null(lu_name_, 'DELIVERY_TYPE', newrec_.delivery_type);
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
   newrec_ IN OUT trn_customer_order_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
   oldrec_ trn_customer_order_tab%ROWTYPE;
BEGIN
   Check_Common___(oldrec_, newrec_, indrec_, attr_);
END Check_Insert___;


-- Insert___
--   Insert a record to the database.
PROCEDURE Insert___ (
   objid_      OUT    VARCHAR2,
   objversion_ OUT    VARCHAR2,
   newrec_     IN OUT trn_customer_order_tab%ROWTYPE,
   attr_       IN OUT VARCHAR2 )
IS
   value_too_large  EXCEPTION;
   PRAGMA           EXCEPTION_INIT(value_too_large, -12899);
BEGIN
   newrec_.rowversion := sysdate;
   newrec_.rowkey := sys_guid();
   Client_SYS.Add_To_Attr('OBJKEY', newrec_.rowkey, attr_);
   newrec_.rowstate := '<UNDEFINED>';
   INSERT
      INTO trn_customer_order_tab
      VALUES newrec_
      RETURNING rowid INTO objid_;
   newrec_.rowstate := NULL;
   Finite_State_Init___(newrec_, attr_);
   objversion_ := to_char(newrec_.rowversion,'YYYYMMDDHH24MISS');
EXCEPTION
   WHEN dup_val_on_index THEN
      DECLARE
         constraint_ VARCHAR2(4000) := Utility_SYS.Get_Constraint_From_Error_Msg(sqlerrm);
      BEGIN
         IF (constraint_ = 'TRN_CUSTOMER_ORDER_RK') THEN
            Error_SYS.Fnd_Rowkey_Exist(lu_name_, newrec_.rowkey);
         ELSIF (constraint_ = 'TRN_CUSTOMER_ORDER_PK') THEN
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
   newrec_ IN OUT trn_customer_order_tab%ROWTYPE )
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
   newrec_ IN OUT trn_customer_order_tab%ROWTYPE )
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
   oldrec_ IN     trn_customer_order_tab%ROWTYPE,
   newrec_ IN OUT trn_customer_order_tab%ROWTYPE,
   indrec_ IN OUT Indicator_Rec,
   attr_   IN OUT VARCHAR2 )
IS
BEGIN
   Validate_SYS.Item_Update(lu_name_, 'COMPANY_ID', indrec_.company_id);
   Validate_SYS.Item_Update(lu_name_, 'BRANCH_ID', indrec_.branch_id);
   Validate_SYS.Item_Update(lu_name_, 'ORDER_ID', indrec_.order_id);
   Validate_SYS.Item_Update(lu_name_, 'ORDER_DATE', indrec_.order_date);
   Validate_SYS.Item_Update(lu_name_, 'CUSTOMER_ID', indrec_.customer_id);
   Check_Common___(oldrec_, newrec_, indrec_, attr_);
END Check_Update___;


-- Update___
--   Update a record in database with new data.
PROCEDURE Update___ (
   objid_      IN     VARCHAR2,
   oldrec_     IN     trn_customer_order_tab%ROWTYPE,
   newrec_     IN OUT trn_customer_order_tab%ROWTYPE,
   attr_       IN OUT VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   by_keys_    IN     BOOLEAN DEFAULT FALSE )
IS
   value_too_large  EXCEPTION;
   PRAGMA           EXCEPTION_INIT(value_too_large, -12899);
BEGIN
   newrec_.rowversion := sysdate;
   IF by_keys_ THEN
      UPDATE trn_customer_order_tab
         SET ROW = newrec_
         WHERE company_id = newrec_.company_id
         AND   branch_id = newrec_.branch_id
         AND   order_id = newrec_.order_id;
   ELSE
      UPDATE trn_customer_order_tab
         SET ROW = newrec_
         WHERE rowid = objid_;
   END IF;
   objversion_ := to_char(newrec_.rowversion,'YYYYMMDDHH24MISS');
EXCEPTION
   WHEN dup_val_on_index THEN
      DECLARE
         constraint_ VARCHAR2(4000) := Utility_SYS.Get_Constraint_From_Error_Msg(sqlerrm);
      BEGIN
         IF (constraint_ = 'TRN_CUSTOMER_ORDER_RK') THEN
            Error_SYS.Fnd_Rowkey_Exist(Trn_Customer_Order_API.lu_name_, newrec_.rowkey);
         ELSIF (constraint_ = 'TRN_CUSTOMER_ORDER_PK') THEN
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
   newrec_         IN OUT trn_customer_order_tab%ROWTYPE,
   lock_mode_wait_ IN     BOOLEAN DEFAULT TRUE )
IS
   objid_      VARCHAR2(20);
   objversion_ VARCHAR2(100);
   attr_       VARCHAR2(32000);
   indrec_     Indicator_rec;
   oldrec_     trn_customer_order_tab%ROWTYPE;
BEGIN
   IF (lock_mode_wait_) THEN
      oldrec_ := Lock_By_Keys___(newrec_.company_id, newrec_.branch_id, newrec_.order_id);
   ELSE
      oldrec_ := Lock_By_Keys_Nowait___(newrec_.company_id, newrec_.branch_id, newrec_.order_id);
   END IF;
   indrec_ := Get_Indicator_Rec___(oldrec_, newrec_);
   Check_Update___(oldrec_, newrec_, indrec_, attr_);
   Update___(objid_, oldrec_, newrec_, attr_, objversion_, TRUE);
END Modify___;


-- Check_Delete___
--   Perform validations on a new record before it is deleted.
PROCEDURE Check_Delete___ (
   remrec_ IN trn_customer_order_tab%ROWTYPE )
IS
   key_ VARCHAR2(2000);
BEGIN
   key_ := remrec_.company_id||'^'||remrec_.branch_id||'^'||remrec_.order_id||'^';
   Reference_SYS.Check_Restricted_Delete(lu_name_, key_);
END Check_Delete___;


-- Delete___
--   Delete a record from the database.
PROCEDURE Delete___ (
   objid_  IN VARCHAR2,
   remrec_ IN trn_customer_order_tab%ROWTYPE )
IS
   key_ VARCHAR2(2000);
BEGIN
   key_ := remrec_.company_id||'^'||remrec_.branch_id||'^'||remrec_.order_id||'^';
   Reference_SYS.Do_Cascade_Delete(lu_name_, key_);
   IF (objid_ IS NOT NULL) THEN
      DELETE
         FROM  trn_customer_order_tab
         WHERE rowid = objid_;
   ELSE
      DELETE
         FROM  trn_customer_order_tab
         WHERE company_id = remrec_.company_id
         AND   branch_id = remrec_.branch_id
         AND   order_id = remrec_.order_id;
   END IF;
END Delete___;


-- Delete___
--   Delete a record from the database.
@Deprecated
PROCEDURE Delete___ (
   remrec_ IN trn_customer_order_tab%ROWTYPE )
IS
BEGIN
   Delete___(NULL, remrec_);
END Delete___;


-- Remove___
--    Removes an existing instance of the logical unit.
PROCEDURE Remove___ (
   remrec_         IN OUT trn_customer_order_tab%ROWTYPE,
   lock_mode_wait_ IN     BOOLEAN DEFAULT TRUE )
IS
   oldrec_     trn_customer_order_tab%ROWTYPE;
BEGIN
   IF (lock_mode_wait_) THEN
      oldrec_ := Lock_By_Keys___(remrec_.company_id, remrec_.branch_id, remrec_.order_id);
   ELSE
      oldrec_ := Lock_By_Keys_Nowait___(remrec_.company_id, remrec_.branch_id, remrec_.order_id);
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
   dummy_ trn_customer_order_tab%ROWTYPE;
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
   newrec_   trn_customer_order_tab%ROWTYPE;
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
   oldrec_   trn_customer_order_tab%ROWTYPE;
   newrec_   trn_customer_order_tab%ROWTYPE;
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
   remrec_ trn_customer_order_tab%ROWTYPE;
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
   rowkey_ IN VARCHAR2 ) RETURN trn_customer_order_tab%ROWTYPE
IS
   rec_ trn_customer_order_tab%ROWTYPE;
BEGIN
   IF (rowkey_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT company_id, branch_id, order_id
      INTO  rec_.company_id, rec_.branch_id, rec_.order_id
      FROM  trn_customer_order_tab
      WHERE rowkey = rowkey_;
   RETURN rec_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN rec_;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(rec_.company_id, rec_.branch_id, rec_.order_id, 'Get_Key_By_Rowkey');
END Get_Key_By_Rowkey;


-- Exist
--   Checks if given pointer (e.g. primary key) to an instance of this
--   logical unit exists. If not an exception will be raised.
@UncheckedAccess
PROCEDURE Exist (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER )
IS
BEGIN
   IF (NOT Check_Exist___(company_id_, branch_id_, order_id_)) THEN
      Raise_Record_Not_Exist___(company_id_, branch_id_, order_id_);
   END IF;
END Exist;


-- Exists
--   Same check as Exist, but returns a BOOLEAN value instead of exception.
@UncheckedAccess
FUNCTION Exists (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN BOOLEAN
IS
BEGIN
   RETURN Check_Exist___(company_id_, branch_id_, order_id_);
END Exists;

-- Rowkey_Exist
--   Checks whether the rowkey exists
--   If not an exception will be raised.
@UncheckedAccess
PROCEDURE Rowkey_Exist (
   rowkey_ IN VARCHAR2 )
IS
   company_id_ trn_customer_order_tab.company_id%TYPE;
   branch_id_ trn_customer_order_tab.branch_id%TYPE;
   order_id_ trn_customer_order_tab.order_id%TYPE;
BEGIN
   IF (rowkey_ IS NULL) THEN
      RAISE no_data_found;
   END IF;
   SELECT company_id, branch_id, order_id
   INTO  company_id_, branch_id_, order_id_
   FROM  trn_customer_order_tab
   WHERE rowkey = rowkey_;
EXCEPTION
   WHEN no_data_found THEN
      Raise_Record_Not_Exist___(company_id_, branch_id_, order_id_);
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, branch_id_, order_id_, 'Rowkey_Exist___');
END Rowkey_Exist;


-- Get_Customer_Id
--   Fetches the CustomerId attribute for a record.
@UncheckedAccess
FUNCTION Get_Customer_Id (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN NUMBER
IS
   temp_ trn_customer_order_tab.customer_id%TYPE;
BEGIN
   IF (company_id_ IS NULL OR branch_id_ IS NULL OR order_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT customer_id
      INTO  temp_
      FROM  trn_customer_order_tab
      WHERE company_id = company_id_
      AND   branch_id = branch_id_
      AND   order_id = order_id_;
   RETURN temp_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, branch_id_, order_id_, 'Get_Customer_Id');
END Get_Customer_Id;


-- Get_Delivery_Type
--   Fetches the DeliveryType attribute for a record.
@UncheckedAccess
FUNCTION Get_Delivery_Type (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN VARCHAR2
IS
   temp_ trn_customer_order_tab.delivery_type%TYPE;
BEGIN
   IF (company_id_ IS NULL OR branch_id_ IS NULL OR order_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT delivery_type
      INTO  temp_
      FROM  trn_customer_order_tab
      WHERE company_id = company_id_
      AND   branch_id = branch_id_
      AND   order_id = order_id_;
   RETURN Trn_Order_Del_Type_API.Decode(temp_);
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, branch_id_, order_id_, 'Get_Delivery_Type');
END Get_Delivery_Type;


-- Get_Delivery_Type_Db
--   Fetches the DB value of DeliveryType attribute for a record.
@UncheckedAccess
FUNCTION Get_Delivery_Type_Db (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN trn_customer_order_tab.delivery_type%TYPE
IS
   temp_ trn_customer_order_tab.delivery_type%TYPE;
BEGIN
   IF (company_id_ IS NULL OR branch_id_ IS NULL OR order_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT delivery_type
      INTO  temp_
      FROM  trn_customer_order_tab
      WHERE company_id = company_id_
      AND   branch_id = branch_id_
      AND   order_id = order_id_;
   RETURN temp_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, branch_id_, order_id_, 'Get_Delivery_Type_Db');
END Get_Delivery_Type_Db;


-- Get_State
--   Fetches the State attribute for a record.
@UncheckedAccess
FUNCTION Get_State (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN VARCHAR2
IS
BEGIN
   RETURN Finite_State_Decode__(Get_Objstate(company_id_, branch_id_, order_id_));
END Get_State;


-- Get_Objstate
--   Fetches the Objstate attribute for a record.
@UncheckedAccess
FUNCTION Get_Objstate (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN VARCHAR2
IS
   temp_ trn_customer_order_tab.rowstate%TYPE;
BEGIN
   IF (company_id_ IS NULL OR branch_id_ IS NULL OR order_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT rowstate
      INTO  temp_
      FROM  trn_customer_order_tab
      WHERE company_id = company_id_
      AND   branch_id = branch_id_
      AND   order_id = order_id_;
   RETURN temp_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, branch_id_, order_id_, 'Get_Objstate');
END Get_Objstate;


-- Get_Objevents
--   Fetches the Objevents attribute for a record.
@UncheckedAccess
FUNCTION Get_Objevents (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN VARCHAR2
IS
BEGIN
   RETURN Finite_State_Events__(Get_Objstate(company_id_, branch_id_, order_id_));
END Get_Objevents;

-- Get_By_Rowkey
--   Fetches a record containing the public attributes by rowkey inparameter.
@UncheckedAccess
FUNCTION Get_By_Rowkey (
   rowkey_ IN VARCHAR2 ) RETURN Public_Rec
IS
   rowrec_ trn_customer_order_tab%ROWTYPE;
BEGIN
   rowrec_ := Get_Key_By_Rowkey(rowkey_);
   RETURN Get(rowrec_.company_id, rowrec_.branch_id, rowrec_.order_id);
END Get_By_Rowkey;

-- Get
--   Fetches a record containing the public attributes.
@UncheckedAccess
FUNCTION Get (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN Public_Rec
IS
   temp_ Public_Rec;
BEGIN
   IF (company_id_ IS NULL OR branch_id_ IS NULL OR order_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT company_id, branch_id, order_id, rowid, rowversion, rowkey, rowstate,
          customer_id, 
          delivery_type
      INTO  temp_
      FROM  trn_customer_order_tab
      WHERE company_id = company_id_
      AND   branch_id = branch_id_
      AND   order_id = order_id_;
   RETURN temp_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, branch_id_, order_id_, 'Get');
END Get;


-- Get_Objkey
--   Fetches the objkey attribute for a record.
@UncheckedAccess
FUNCTION Get_Objkey (
   company_id_ IN VARCHAR2,
   branch_id_ IN VARCHAR2,
   order_id_ IN NUMBER ) RETURN VARCHAR2
IS
   rowkey_ trn_customer_order_tab.rowkey%TYPE;
BEGIN
   IF (company_id_ IS NULL OR branch_id_ IS NULL OR order_id_ IS NULL) THEN
      RETURN NULL;
   END IF;
   SELECT rowkey
      INTO  rowkey_
      FROM  trn_customer_order_tab
      WHERE company_id = company_id_
      AND   branch_id = branch_id_
      AND   order_id = order_id_;
   RETURN rowkey_;
EXCEPTION
   WHEN no_data_found THEN
      RETURN NULL;
   WHEN too_many_rows THEN
      Raise_Too_Many_Rows___(company_id_, branch_id_, order_id_, 'Get_Objkey');
END Get_Objkey;

-------------------- FINITE STATE MACHINE -----------------------------------

-- Get_Db_Values___
--   Returns the the list of DB (stored in database) values.
FUNCTION Get_Db_Values___ RETURN VARCHAR2 DETERMINISTIC
IS
BEGIN
   RETURN('Opened^Closed^Invoiced^Cancelled^');
END Get_Db_Values___;


-- Get_Client_Values___
--   Returns the the list of client (in PROG language) values.
FUNCTION Get_Client_Values___ RETURN VARCHAR2 DETERMINISTIC
IS
BEGIN
   RETURN('Opened^Closed^Invoiced^Cancelled^');
END Get_Client_Values___;


-- Close_Order_Lines___
--    Execute the CloseOrderLines action within the finite state machine.
PROCEDURE Close_Order_Lines___ (
   rec_  IN OUT trn_customer_order_tab%ROWTYPE,
   attr_ IN OUT VARCHAR2 );


-- Check_Order_Lines___
--    Evaluates the CheckOrderLines condition within the finite state machine.
FUNCTION Check_Order_Lines___ (
   rec_  IN     trn_customer_order_tab%ROWTYPE ) RETURN BOOLEAN;


-- Finite_State_Set___
--    Updates the state column in the database for given record.
PROCEDURE Finite_State_Set___ (
   rec_   IN OUT trn_customer_order_tab%ROWTYPE,
   state_ IN     VARCHAR2 )
IS
BEGIN
   rec_.rowversion := sysdate;
   UPDATE trn_customer_order_tab
      SET rowstate = state_,
          rowversion = rec_.rowversion
      WHERE company_id = rec_.company_id
      AND   branch_id = rec_.branch_id
      AND   order_id = rec_.order_id;
   rec_.rowstate := state_;
END Finite_State_Set___;


-- Finite_State_Machine___
--    Execute the state machine logic given a specific event.
PROCEDURE Finite_State_Machine___ (
   rec_   IN OUT trn_customer_order_tab%ROWTYPE,
   event_ IN     VARCHAR2,
   attr_  IN OUT VARCHAR2 )
IS
   state_ trn_customer_order_tab.rowstate%TYPE;
BEGIN
   state_ := rec_.rowstate;
   IF (state_ IS NULL) THEN
      IF (event_ IS NULL) THEN
         Finite_State_Set___(rec_, 'Opened');
      ELSE
         Error_SYS.State_Event_Not_Handled(lu_name_, event_, Finite_State_Decode__(state_));
      END IF;
   ELSIF (state_ = 'Cancelled') THEN
      Error_SYS.State_Event_Not_Handled(lu_name_, event_, Finite_State_Decode__(state_));
   ELSIF (state_ = 'Closed') THEN
      IF (event_ = 'Invoice') THEN
         IF (Check_Order_Lines___(rec_)) THEN
            Close_Order_Lines___(rec_, attr_);
            Finite_State_Set___(rec_, 'Invoiced');
         END IF;
      ELSE
         Error_SYS.State_Event_Not_Handled(lu_name_, event_, Finite_State_Decode__(state_));
      END IF;
   ELSIF (state_ = 'Invoiced') THEN
      Error_SYS.State_Event_Not_Handled(lu_name_, event_, Finite_State_Decode__(state_));
   ELSIF (state_ = 'Opened') THEN
      IF (event_ = 'Cancel') THEN
         Finite_State_Set___(rec_, 'Cancelled');
      ELSIF (event_ = 'Close') THEN
         Finite_State_Set___(rec_, 'Closed');
      ELSE
         Error_SYS.State_Event_Not_Handled(lu_name_, event_, Finite_State_Decode__(state_));
      END IF;
   ELSE
      Error_SYS.State_Not_Exist(lu_name_, Finite_State_Decode__(state_));
   END IF;
END Finite_State_Machine___;


-- Finite_State_Add_To_Attr___
--    Add current state and lists of allowed events to an attribute string.
PROCEDURE Finite_State_Add_To_Attr___ (
   rec_  IN     trn_customer_order_tab%ROWTYPE,
   attr_ IN OUT VARCHAR2 )
IS
   state_ trn_customer_order_tab.rowstate%TYPE;
BEGIN
   state_ := rec_.rowstate;
   Client_SYS.Add_To_Attr('__OBJSTATE', state_, attr_);
   Client_SYS.Add_To_Attr('__OBJEVENTS', Finite_State_Events__(state_), attr_);
   Client_SYS.Add_To_Attr('STATE', Finite_State_Decode__(state_), attr_);
END Finite_State_Add_To_Attr___;


-- Finite_State_Init___
--    Runs the initial start event for the state machine.
PROCEDURE Finite_State_Init___ (
   rec_  IN OUT trn_customer_order_tab%ROWTYPE,
   attr_ IN OUT VARCHAR2 )
IS
BEGIN
   Finite_State_Machine___(rec_, NULL, attr_);
   Finite_State_Add_To_Attr___(rec_, attr_);
END Finite_State_Init___;


-- Finite_State_Init_
--    Runs the initial start event for a basedOn child entity.
@ServerOnlyAccess
PROCEDURE Finite_State_Init_ (
   rec_  IN OUT trn_customer_order_tab%ROWTYPE,
   attr_ IN OUT VARCHAR2 )
IS
BEGIN
   Finite_State_Init___(rec_, attr_);
END Finite_State_Init_;


-- Finite_State_Decode__
--   Returns the client equivalent for any database representation of
--   a state name = objstate.
@UncheckedAccess
FUNCTION Finite_State_Decode__ (
   db_state_ IN VARCHAR2 ) RETURN VARCHAR2
IS
BEGIN
   RETURN(Domain_SYS.Decode_(Domain_SYS.Get_Translated_Values(lu_name_), Get_Db_Values___, db_state_));
END Finite_State_Decode__;


-- Finite_State_Encode__
--   Returns the database equivalent for any client representation of
--   a state name = state.
@UncheckedAccess
FUNCTION Finite_State_Encode__ (
   client_state_ IN VARCHAR2 ) RETURN VARCHAR2
IS
BEGIN
   RETURN(Domain_SYS.Encode_(Domain_SYS.Get_Translated_Values(lu_name_), Get_Db_Values___, client_state_));
END Finite_State_Encode__;


-- Enumerate_States__
--   Returns a list of all possible finite states in client terminology.
@UncheckedAccess
PROCEDURE Enumerate_States__ (
   client_values_ OUT VARCHAR2 )
IS
BEGIN
   client_values_ := Domain_SYS.Enumerate_(Domain_SYS.Get_Translated_Values(lu_name_));
END Enumerate_States__;


-- Enumerate_States_Db__
--   Returns a list of all possible finite states in database terminology.
@UncheckedAccess
PROCEDURE Enumerate_States_Db__ (
   db_values_ OUT VARCHAR2 )
IS
BEGIN
   db_values_ := Domain_SYS.Enumerate_(Get_Db_Values___);
END Enumerate_States_Db__;


-- Finite_State_Events__
--   Returns a list of allowed events for a given state
--   NOTE! Regardless of conditions if not otherwize encoded
@UncheckedAccess
FUNCTION Finite_State_Events__ (
   db_state_ IN VARCHAR2 ) RETURN VARCHAR2
IS
BEGIN
   IF (db_state_ IS NULL) THEN
      RETURN NULL;
   ELSIF (db_state_ = 'Cancelled') THEN
      RETURN NULL;
   ELSIF (db_state_ = 'Closed') THEN
      RETURN 'Invoice^';
   ELSIF (db_state_ = 'Invoiced') THEN
      RETURN NULL;
   ELSIF (db_state_ = 'Opened') THEN
      RETURN 'Close^Cancel^';
   ELSE
      RETURN NULL;
   END IF;
END Finite_State_Events__;


-- Enumerate_Events__
--   Returns a list of all possible events.
@UncheckedAccess
PROCEDURE Enumerate_Events__ (
   db_events_ OUT VARCHAR2 )
IS
BEGIN
   db_events_ := 'Cancel^Close^Invoice^';
END Enumerate_Events__;


-- Cancel__
--   Executes the Cancel event logic as defined in the state machine.
PROCEDURE Cancel__ (
   info_       OUT    VARCHAR2,
   objid_      IN     VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   attr_       IN OUT VARCHAR2,
   action_     IN     VARCHAR2 )
IS
   rec_ trn_customer_order_tab%ROWTYPE;
BEGIN
   IF (action_ = 'CHECK') THEN
      NULL;
   ELSIF (action_ = 'DO') THEN
      rec_ := Lock_By_Id___(objid_, objversion_);
      Finite_State_Machine___(rec_, 'Cancel', attr_);
      objversion_ := to_char(rec_.rowversion,'YYYYMMDDHH24MISS');
      Finite_State_Add_To_Attr___(rec_, attr_);
   END IF;
   info_ := Client_SYS.Get_All_Info;
END Cancel__;


-- Close__
--   Executes the Close event logic as defined in the state machine.
PROCEDURE Close__ (
   info_       OUT    VARCHAR2,
   objid_      IN     VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   attr_       IN OUT VARCHAR2,
   action_     IN     VARCHAR2 )
IS
   rec_ trn_customer_order_tab%ROWTYPE;
BEGIN
   IF (action_ = 'CHECK') THEN
      NULL;
   ELSIF (action_ = 'DO') THEN
      rec_ := Lock_By_Id___(objid_, objversion_);
      Finite_State_Machine___(rec_, 'Close', attr_);
      objversion_ := to_char(rec_.rowversion,'YYYYMMDDHH24MISS');
      Finite_State_Add_To_Attr___(rec_, attr_);
   END IF;
   info_ := Client_SYS.Get_All_Info;
END Close__;


-- Invoice__
--   Executes the Invoice event logic as defined in the state machine.
PROCEDURE Invoice__ (
   info_       OUT    VARCHAR2,
   objid_      IN     VARCHAR2,
   objversion_ IN OUT VARCHAR2,
   attr_       IN OUT VARCHAR2,
   action_     IN     VARCHAR2 )
IS
   rec_ trn_customer_order_tab%ROWTYPE;
BEGIN
   IF (action_ = 'CHECK') THEN
      NULL;
   ELSIF (action_ = 'DO') THEN
      rec_ := Lock_By_Id___(objid_, objversion_);
      Finite_State_Machine___(rec_, 'Invoice', attr_);
      objversion_ := to_char(rec_.rowversion,'YYYYMMDDHH24MISS');
      Finite_State_Add_To_Attr___(rec_, attr_);
   END IF;
   info_ := Client_SYS.Get_All_Info;
END Invoice__;



-------------------- COMPLEX STRUCTURE METHODS ------------------------------------

-------------------- FOUNDATION1 METHODS ------------------------------------


-- Language_Refreshed
--   Framework method that updates translations to a new language.
@UncheckedAccess
PROCEDURE Language_Refreshed
IS
BEGIN
   Domain_SYS.Language_Refreshed(lu_name_, Get_Client_Values___, Get_Db_Values___, 'STATE');
END Language_Refreshed;


-- Init
--   Framework method that initializes this package.
@UncheckedAccess
PROCEDURE Init
IS
BEGIN
   Domain_SYS.Load_State(lu_name_, Get_Client_Values___, Get_Db_Values___);
END Init;