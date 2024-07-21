      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0661.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRKI0661.CPv  $
      *
      *   Rev 2.1   Jan 08 2003 12:09:26   t13ex27
      *AKI00299 - Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 12:58:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   25 Aug 1998 09:28:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0661 FUNKTION:VERTRAGSDATEN ERMITTELN    *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:    10.09.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00215! 1.7.01 ! 1.0! NEU                       ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0661                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ITEM, ITEMBT, MITARBEITER, ERMITTELN LIST       *
      *              - ALLE ITEM-Eintraege mit bestimmtem Status     *
      *                ERMITTELN                                     *
      *              - Alle IBTVBNR zum jeweiligen ITEM-Eintrag      *
      *                ERMITTELN                                     *
      *              - MITARBPERSONENID zur IBTVBNR ERMITTELN        *

      ****************************************************************
       01 PI-DRKI0661.
           05  PI-DRKI0661-PROGRAMM             PIC  X(008).
           05  PI-DRKI0661-UFKT-CODE            PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI0661-DATEN.
             10  PI-DRKI0661-NATPHBPERSONENID   PIC  X(013).

      ***
       01 PO-DRKI0661.
          05 PO-DRKI0661-RC                     PIC  X(003).
             88 PO-DRKI0661-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0661-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0661-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI0661-DATEN.
             10 PO-DRKI0661-NATPHBHHPERSONENID  PIC X(13).
      *
      ******** Ende Copy-Member **************************************

