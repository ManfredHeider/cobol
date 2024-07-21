      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1098.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1098.CPv  $
      *
      *   Rev 2.1   06 Jul 2000 13:27:14   T13EX39
      *AKI00100 BFG-INVEST
      *
      *   Rev 1.1   08 Feb 1999 18:22:52   T13EX47
      *Alle Vertraege eines VNs fuer LIST-OZP ZXKI0359
      *
      *   Rev 1.0   06 Oct 1998 15:31:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1098                                      *
      *    --------------------------------------------------------   *
      *                                                               *
      *                                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WHO                                              *
      *    DATUM:     5.06.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00100!24.03.00! 01 ! NEUERSTELLUNG             ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1098                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1098.
          05 PI-DRKI1098-PROGRAMM                 PIC X(008).
          05 PI-DRKI1098-UFKT-CODE                PIC 9(002).
          05 PI-DRKI1098-DATEN.
            07 PI-DRKI1098-VRBVERTRAGSID          PIC X(013).
            07 PI-DRKI1098-PERSONENROLLEKZ        PIC X(001).
      *
       01 PO-DRKI1098.

          05 PO-DRKI1098-RC                       PIC X(003).
             88 PO-DRKI1098-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1098-RC-LEER                  VALUE 'LER'.
             88 PO-DRKI1098-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI1098-DATEN.
             10 PO-DRKI1098-VRBVERTRAGSID               PIC X(13).
             10 PO-DRKI1098-VRBPERSONENID               PIC X(13).
             10 PO-DRKI1098-VRBPERSONENROLLEKZ          PIC X(01).
             10 PO-DRKI1098-VRBPABPERSONENID            PIC X(13).
             10 PO-DRKI1098-VRBPABADRESSEID             PIC X(13).
             10 PO-DRKI1098-VRBABLAUFDATUM              PIC X(10).
             10 PO-DRKI1098-VRBGUELTIGKEITSDAT          PIC X(10).
             10 PO-DRKI1098-VRBAVKENNZEICHEN            PIC X(01).
      *
      ***** Ende Copy-Member DRKI1098 *****************************
