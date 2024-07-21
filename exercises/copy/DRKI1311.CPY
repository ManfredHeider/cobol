      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1311.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1311.CPv  $
      *
      *   Rev 2.1   Feb 27 2007 12:48:40   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.0   May 11 2006 09:25:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!10.03.06!2.1 ! NEUERSTELLUNG             ! I.Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRKI1311.

          05 PI-DRKI1311-PROGRAMM                 PIC X(008).
          05 PI-DRKI1311-UFKT-CODE                PIC 9(002).
          05 PI-DRKI1311-DATEN.
            07 PI-DRKI1311-VRBPERSONENID          PIC X(013).
            07 PI-DRKI1311-VRBPERSONENROLLEKZ     PIC X(001).
      *
       01 PO-DRKI1311.

          05 PO-DRKI1311-RC                       PIC X(003).
             88 PO-DRKI1311-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1311-RC-LEER                  VALUE 'LER'.
             88 PO-DRKI1311-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI1311-DATEN.
             10 PO-DRKI1311-VRBVERTRAGSID               PIC X(013).
             10 PO-DRKI1311-VRBPABPERSONENID            PIC X(013).
             10 PO-DRKI1311-VRBPABADRESSEID             PIC X(013).
             10 PO-DRKI1311-VRBABLAUFDATUM              PIC X(010).
             10 PO-DRKI1311-VRBGUELTIGKEITSDAT          PIC X(010).
             10 PO-DRKI1311-VRBAVKENNZEICHEN            PIC X(001).
             10 PO-DRKI1311-LETZTEAENDERUNG             PIC X(026).
             10 PO-DRKI1311-VRBKOMMBETRKZ               PIC X(01).
      *
      ******* ENDE COPY-MEMBER *****************************************
