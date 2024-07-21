      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0814.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0814.CPv  $
      *
      *   Rev 2.1   Jul 27 2005 13:03:36   t13ae31
      *AKI00469: Anpassungen Central-DTA
      *
      *   Rev 2.0   Apr 15 1999 11:43:32   T13EX50
      *Produktionsübergabe - CKV
      *
      *   Rev 1.1   Mar 25 1999 10:32:06   T13EX50
      *Erste Version 
      *
      *   Rev 1.0   12 Jan 1999 15:48:20   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *01.02.99!NEUERSTELLUNG                          !HERWEG   ! 01  *
      *AKI00022!                                       !         !     *
      *        !                                       !         !     *
      *28.02.05!                                       !         !     *
      *AKI00469! neue DTA-Schnittstelle                ! Hübner  !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0814.
280205***    10 VERB-KEY-VNR-UNR.
280205***       20 VERB-VNR                PIC X(14).
280205***       20 VERB-UNR                PIC 9(02).
280205***    10 VERB-KEY-VNR-PNRVN.
280205***       20 VERB-VNR-AKEY           PIC X(14).
280205***       20 VERB-PNRVN-AKEY         PIC 9(03).
280205***    10 VERB-PNRVN                 PIC 9(03).
280205     10 VERB-KEY-VNR-PNR.
280205        20 VERB-VNR                PIC 9(09).
280205        20 VERB-PNR                PIC 9(04).
280205     10 VERB-KEY-VNR-PNRVN.
280205        20 VERB-VNR-AKEY           PIC 9(09).
280205        20 VERB-PNRVN-AKEY         PIC 9(04).
280205     10 VERB-PNRVN                 PIC 9(04).
