      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0962.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0962.CPv  $
      *
      *   Rev 2.1   31 Jan 2001 14:29:44   T13EX39
      *AKI00068 KBI
      *
      *   Rev 1.1   08 Feb 1999 18:22:52   T13EX47
      *Alle Vertraege eines VNs fuer LIST-OZP ZXKI0359
      *
      *   Rev 1.0   06 Oct 1998 15:31:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0962                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0359                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WHO                                              *
      *    DATUM:    27.01.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00068!27.01.00! 01 ! NEUERSTELLUNG             ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0962                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0962.
          05 PI-DRKI0962-PROGRAMM                 PIC X(008).
          05 PI-DRKI0962-UFKT-CODE                PIC 9(002).
          05 PI-DRKI0962-DATEN.
            07 PI-DRKI0962-VRBPERSONENID          PIC X(013).
      *
       01 PO-DRKI0962.

          05 PO-DRKI0962-RC                       PIC X(003).
             88 PO-DRKI0962-RC-OK                    VALUE 'OK '.
             88 PO-DRKI0962-RC-LEER                  VALUE 'LER'.
             88 PO-DRKI0962-RC-EOF                   VALUE 'EOF'.


          05 PO-DRKI0962-DATEN.
             10 PO-DRKI0962-VRBVERTRAGSID               PIC X(13).
             10 PO-DRKI0962-VRBPERSONENID               PIC X(13).
             10 PO-DRKI0962-VRBPERSONENROLLEKZ          PIC X(01).
             10 PO-DRKI0962-VRBPABPERSONENID            PIC X(13).
             10 PO-DRKI0962-VRBPABADRESSEID             PIC X(13).
      *
      ***** Ende Copy-Member DRKI0962 *****************************
