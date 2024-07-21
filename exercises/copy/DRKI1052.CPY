      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1052.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRKI1052.CPv  $
      *
      *   Rev 2.2   Jan 21 2002 13:24:04   T13EX39
      *FKI00565 TEIL-EA
      *
      *   Rev 2.1   Mar 30 2000 13:05:06   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:19:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * -------------------------------------------------------------*
      * AKI00565!21.01.02!    ! FEHLERBESEITIGUNG TEIL-EA ! ABO/WHO  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *
      ****************************************************************
       01 PI-DRKI1052.
           05  PI-DRKI1052-PERSID    PIC X(13).

       01 PO-DRKI1052.
          05 PO-DRKI1052-RC                    PIC  X(003).
             88 PO-DRKI1052-RC-OK                         VALUE 'OK '.
             88 PO-DRKI1052-RC-NOK                        VALUE 'NOK'.

           05  PO-DRKI1052-DATEN-ANZ                PIC S9(4) COMP.
           05  PO-DRKI1052-DATEN-ALL.
             08  PO-DRKI1052-DATEN  OCCURS 300.
               10 PO-DRKI1052-PTRTABLENAME.
                  49 PO-DRKI1052-PTRTABLENAME-LEN   PIC S9(4) COMP.
                  49 PO-DRKI1052-PTRTABLENAME-TEXT  PIC X(40).
               10 PO-DRKI1052-PTRKEYDATA.
                  49 PO-DRKI1052-PTRKEYDATA-LEN     PIC S9(4) COMP.
                  49 PO-DRKI1052-PTRKEYDATA-TEXT    PIC X(200).
               10 PO-DRKI1052-PTRSQLDATA.
                  49 PO-DRKI1052-PTRSQLDATA-LEN     PIC S9(4) COMP.
                  49 PO-DRKI1052-PTRSQLDATA-TEXT    PIC X(3600).

      ****************************************************************
