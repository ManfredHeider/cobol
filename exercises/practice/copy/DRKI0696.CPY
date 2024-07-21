      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0696.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0696.CPv  $
      *
      *   Rev 2.1   Jun 21 1999 11:15:36   T13EX50
      *Anpassen an die neue Satzstruktur 
      *
      *   Rev 2.0   Apr 15 1999 11:41:44   T13EX50
      *Produktionsübergabe - CKV
      *
      *   Rev 1.1   Mar 25 1999 10:33:26   T13EX50
      *Erste Version 
      *
      *   Rev 1.0   25 Aug 1998 09:31:32   P13SP99
      *Initial revision.
      *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *01.02.99!NEUERSTELLUNG                          !HERWEG   ! 01  *
      *AKI00022!                                       !         !     *
      *        !                                       !         !     *
      *31.05.99!EINBAU WHR-KZ                          !HERWEG   ! 1.1 *
      *FKI00042!                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
CCC009*    SKIP1
       01  DRKI0696.
           05  CKV-N-SATZ1.
               10  CKV-N-KEY-FELDER.
                   20  CKV-N-KEY-VNR         PIC X(14).
                   20  CKV-N-KEY-UNR         PIC X(04).
                   20  CKV-N-KEY-LFDNR       PIC 9(04).
               10  CKV-N-RESTDATEN           PIC X(166).

