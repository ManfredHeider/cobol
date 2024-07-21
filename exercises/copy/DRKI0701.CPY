      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0701.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0701.CPv  $
      *
      *   Rev 2.1   Sep 30 2009 14:50:08   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.0   Feb 27 1999 12:58:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   10 Feb 1999 18:19:50   T13EX47
      *SELECT fuer ZXKI0043
      *
      *   Rev 1.0   06 Oct 1998 15:30:36   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0701                                      *
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
      *    FÜR DIE FUNKTION DRKI0701                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0701.
          05 PI-DRKI0701-PROGRAMM           PIC  X(008).
          05 PI-DRKI0701-UFKT-CODE          PIC  9(002).
          05 PI-DRKI0701-DATEN.
310809*     07 PI-DRKI0701-suchname               PIC X(015).
310809*     07 PI-DRKI0701-suchvorname            PIC X(015).
310809      07 PI-DRKI0701-suchname               PIC X(030).
310809      07 PI-DRKI0701-suchvorname            PIC X(030).
            07 PI-DRKI0701-gebgruenddat           PIC X(010).
      *
       01 PO-DRKI0701.

          05 PO-DRKI0701-RC                 PIC  X(003).
             88 PO-DRKI0701-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0701-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0701-RC-EOF                       VALUE 'EOF'.

          05 PO-DRKI0701-DATEN.
            07 PO-DRKI0701-personenid            PIC X(013).
            07 PO-DRKI0701-gebgruenddat          PIC X(010).
      *
      ***** Ende Copy-Member DRKI0701 *****************************
