      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0695.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0695.CPv  $
      *
      *   Rev 2.1   Sep 30 2009 14:50:00   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.0   Feb 27 1999 12:58:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 19:11:58   T13EX47
      *SELECT im LIST-OZP ZXKI0043
      *
      *   Rev 1.0   25 Aug 1998 09:31:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0695                                      *
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
      * AKI00001!13.07.98! 01 ! NEUERSTELLUNG             !   gbe    *
      * -------------------------------------------------------------*
      * AKI00852!31.08.09! 02 ! SUCHNAME AUF 30 STELLEN   ! HRO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0695                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0695.
          05 PI-DRKI0695-PROGRAMM                 PIC  X(008).
          05 PI-DRKI0695-UFKT-CODE                PIC  9(002).
          05 PI-DRKI0695-DATEN.
310809*     07 PI-DRKI0695-suchname               PIC X(015).
310809*     07 PI-DRKI0695-suchvorname            PIC X(015).
310809      07 PI-DRKI0695-suchname               PIC X(030).
310809      07 PI-DRKI0695-suchvorname            PIC X(030).
      *
       01 PO-DRKI0695.

          05 PO-DRKI0695-RC                       PIC  X(003).
             88 PO-DRKI0695-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0695-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0695-RC-EOF                       VALUE 'EOF'.

          05 PO-DRKI0695-DATEN.
            07 PO-DRKI0695-personenid             PIC X(013).
            07 PO-DRKI0695-gebgruenddat           PIC X(010).
      *
      ***** Ende Copy-Member DRKI0695 *****************************
