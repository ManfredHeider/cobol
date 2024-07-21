      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0331.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0331.CPv  $
      *
      *   Rev 2.3   12 Nov 2007 15:47:20   t13ex12
      *FKI01117: Fonds-ID fehlerhaft, in Abgleich und Akt.
      *
      *   Rev 2.2   Nov 28 2002 08:41:14   t13ex49
      *AKI00294 - ISIN
      *
      *   Rev 2.1   Apr 22 1999 13:33:54   T13EX39
      *FKI00018 Feld für  Kontoart
      *
      *   Rev 1.1   08 Feb 1999 16:25:54   T13EX39
      *FUNKTION SELECT FUER ZXKI0307
      *
      *   Rev 1.0   14 Jul 1998 17:00:02   P13SP13
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
      *        !PI-DRKI0331-PAPKZ                      !         !     *
      *FKI00018!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *12.11.07!Feld für Fonds-ID mit COMP definieren  !v.d.Goltz! 03  *
      *        !damit 5-stellige IDs verarbeitet werden.         !     *
      *FKI01117!                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  PI-DRKI0331.
           05  PI-DRKI0331-UFKT                PIC  9(02).
      *
       01  PO-DRKI0331.
           05  PO-DRKI0331-RC                  PIC  X(003).
               88 PO-DRKI0331-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0331-RC-EOF                       VALUE 'EOF'.
               88 PO-DRKI0331-RC-LEER                      VALUE 'LER'.
           05  PO-DRKI0331-ERGEBNISZEILE.
               10 PO-DRKI0331-PAPKNR           PIC  X(06).
121107*        10 PO-DRKI0331-FONDID           PIC S9(04).
121107         10 PO-DRKI0331-FONDID           PIC S9(04) COMP.
               10 PO-DRKI0331-FONDGESNR        PIC S9(04) COMP.
               10 PO-DRKI0331-PAPKZ            PIC  X(01).
181002         10 PO-DRKI0331-FONDISIN         PIC  X(12).

      ******* ENDE COPY-MEMBER **************************************
