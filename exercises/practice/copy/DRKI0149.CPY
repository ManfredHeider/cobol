      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0149.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0149.CPv  $
      *
      *   Rev 2.2   12 Jan 2001 10:51:48   T13EX13
      * 
      *
      *   Rev 2.1   Jan 25 2000 12:20:34   T13EX48
      *AKI00033
      *BT/PÜ
      *
      *   Rev 2.0   Feb 27 1999 12:53:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:52:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0149 FUNKTION:VERTRAGSDATEN ERMITTELN    *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HSE
      *    DATUM:    06.09.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00033!06.09.99! 01 !NEU                        !HSE       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0149                                 *
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
       01 PI-DRKI0149.
           05  PI-DRKI0149-PROGRAMM             PIC  X(008).
           05  PI-DRKI0149-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI0149-DATEN.
             10  PI-DRKI0149-ITMSTATUS            PIC  9(004).

      ***
       01 PO-DRKI0149.
          05 PO-DRKI0149-RC                     PIC  X(003).
             88 PO-DRKI0149-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0149-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0149-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI0149-DATEN.
             10 PO-DRKI0149-ITMID                 PIC  X(013).
             10 PO-DRKI0149-MITARBPERSONENID      PIC  X(013).
             10 PO-DRKI0149-MITARBEITERNR         PIC  9(007).
      *
      ******** Ende Copy-Member **************************************

