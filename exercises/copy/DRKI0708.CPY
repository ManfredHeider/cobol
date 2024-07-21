      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0708.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0708.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 18:22:52   T13EX47
      *Alle Vertraege eines VNs fuer LIST-OZP ZXKI0359
      *
      *   Rev 1.0   06 Oct 1998 15:31:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0708                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXAT0005                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Zgur-Vujic                                       *
      *    DATUM:    13.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!13.07.98! 01 ! NEUERSTELLUNG             !Zgur-Vujic*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0708                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0708.
          05 PI-DRKI0708-PROGRAMM                 PIC X(008).
          05 PI-DRKI0708-UFKT-CODE                PIC 9(002).
          05 PI-DRKI0708-DATEN.
            07 PI-DRKI0708-personenid             PIC X(013).
            07 PI-DRKI0708-personenrollekz        PIC X(001).
      *
       01 PO-DRKI0708.

          05 PO-DRKI0708-RC                       PIC X(003).
             88 PO-DRKI0708-RC-OK                    VALUE 'OK '.
             88 PO-DRKI0708-RC-LEER                  VALUE 'LER'.
             88 PO-DRKI0708-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI0708-DATEN.
            07 PO-DRKI0708-vertragsid             PIC X(013).
            07 PO-DRKI0708-pabadresseid           PIC X(013).
      *
      ***** Ende Copy-Member DRKI0708 *****************************
