      * T13EX39   Stand: 28/07/98    17:05:18
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0320.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRKI0320.CPv  $
      *
      *   Rev 2.1   Jan 21 2002 13:18:00   T13EX39
      *FKI00565 TEIL-EA
      *
      *   Rev 1.1   28 Jan 1999 10:10:02   T13EX39
      *afki0001 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 16:59:16   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *
      * -------------------------------------------------------------*
      * AKI00565!21.01.02!    ! FEHLERBESEITIGUNG TEIL-EA ! ABO/WHO  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *
      ****************************************************************
       01  PI-DRKI0320.
           05  PI-DRKI0320-UFKT               PIC X.
               88 PI-DRKI0320-PERSONID          VALUE 'P'.
               88 PI-DRKI0320-VERTRAGID         VALUE 'V'.
               88 PI-DRKI0320-OBJEKTID          VALUE 'O'.
               88 PI-DRKI0320-ADRESSEID         VALUE 'A'.
               88 PI-DRKI0320-VERTRAGNOTIZ      VALUE 'N'.
           05  PI-DRKI0320-SUCHID             PIC X(13).

      *
       01  PO-DRKI0320.
           05  PO-DRKI0320-VBNUMMER           PIC S9(9) COMP.
           05  PO-DRKI0320-RC                 PIC  X(003).
               88 PO-DRKI0320-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0320-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
