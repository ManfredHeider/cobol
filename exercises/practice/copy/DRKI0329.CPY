      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0329.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0329.CPv  $
      *
      *   Rev 2.1   Apr 22 1999 13:33:42   T13EX39
      *FKI00018 Feld für  Kontoart
      *
      *   Rev 1.1   08 Feb 1999 16:22:38   T13EX39
      *FUNKTION FUER UPKI0043 (ALLE FON-IDS)
      *
      *   Rev 1.0   14 Jul 1998 16:59:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *14.04.99!Feld für Kontoart (DE, VL) definieren. !WHO      ! 02  *
      *FKI00018!PI-DRKI0329-PAPKZ                      !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  PI-DRKI0329.
           05  PI-DRKI0329-DATEN.
               10  PI-DRKI0329-PAPKNR          PIC   X(06).
               10  PI-DRKI0329-GESNR           PIC  S9(04) COMP.
               10  PI-DRKI0329-PAPKZ           PIC   X(01).
      *
       01  PO-DRKI0329.
           05  PO-DRKI0329-RC                  PIC  X(003).
               88 PO-DRKI0329-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0329-RC-NFD                       VALUE 'NFD'.
           05  PO-DRKI0329-ERGEBNISZEILE.
               10 PO-DRKI0329-FONDID           PIC S9(04) comp.

      ******* ENDE COPY-MEMBER **************************************
