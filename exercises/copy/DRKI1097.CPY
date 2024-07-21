      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1097.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1097.CPv  $
      *
      *   Rev 2.2   Nov 25 2009 12:49:58   t13ex39
      *FKI01265: Fehler bei Verarbeitung Attribut VERDATPRODLOESCH
      *
      *   Rev 2.1   06 Jul 2000 13:27:12   T13EX39
      *AKI00100 BFG-INVEST
      *
      *   Rev 2.0   Feb 27 1999 12:53:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:52:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1097 FUNKTION:VERTRAGSDATEN ERMITTELN    *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:     5.06.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00100!24.03.00! 01 !NEU                        !WHO       *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * FKI01265!11.11.09! 1.3! VERDATPRODLOESCHF mit     !WHO       *
      *         !        !    ! aufgenommen               !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1097                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION:                                                 *
      *              - ALLE GUTHABENBUCHUNGEN ZU EINER GESELLSCHAFT  *
      *                ERMITTELN                                     *
      *                                                              *
      ****************************************************************
       01 PI-DRKI1097.
           05  PI-DRKI1097-PROGRAMM             PIC  X(008).
           05  PI-DRKI1097-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI1097-DATEN.
             10  PI-DRKI1097-KEY-VON            PIC  X(013).
             10  PI-DRKI1097-KEY-BIS            PIC  X(013).

      ***
       01 PO-DRKI1097.
          05 PO-DRKI1097-RC                     PIC  X(003).
             88 PO-DRKI1097-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1097-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1097-RC-EOF                       VALUE 'EOF'.
           05  PO-DRKI1097-DATEN.
             10 PO-DRKI1097-VERVERTRAGSID            PIC X(13).
             10 PO-DRKI1097-VERABSCHLUSSGRUND.
                15 PO-DRKI1097-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PO-DRKI1097-VRBSCHLSSGRND-TXT   PIC X(30).

             10 PO-DRKI1097-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI1097-VERAENDERUNGSDATUM   PIC X(26).
             10 PO-DRKI1097-VERDATVERTRAEND   PIC X(10).
             10 PO-DRKI1097-VEREIGENVERMKZ   PIC X(01).
             10 PO-DRKI1097-VERFRALFDNR   PIC S9(4) COMP.
             10 PO-DRKI1097-VERFRAPERSONENID   PIC X(13).
             10 PO-DRKI1097-VERFREISTAUFTRAG   PIC X(01).
             10 PO-DRKI1097-VERFREMDVERTRAGKZ   PIC X(01).
             10 PO-DRKI1097-VERGEBUENDELTKZ   PIC X(01).
             10 PO-DRKI1097-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1097-VERGESBEITRWKZ   PIC X(01).
             10 PO-DRKI1097-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PO-DRKI1097-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PO-DRKI1097-VERNACHLASSKZ   PIC X(01).
             10 PO-DRKI1097-VERNOTIZID   PIC X(13).
             10 PO-DRKI1097-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PO-DRKI1097-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PO-DRKI1097-VERREMDVERTRGESNAM.
                15 PO-DRKI1097-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PO-DRKI1097-VRRMDVRTRGSNM-TXT   PIC X(25).

             10 PO-DRKI1097-VERSTORNODATUM   PIC X(10).
             10 PO-DRKI1097-VERVERTRAGSABLAUF   PIC X(10).
             10 PO-DRKI1097-VERVERTRAGSBEGINN   PIC X(10).
             10 PO-DRKI1097-VERVERTRAGSNUMMER   PIC X(15).
             10 PO-DRKI1097-VERVERTRZUSTANDDAT   PIC X(10).
             10 PO-DRKI1097-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PO-DRKI1097-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PO-DRKI1097-VERZAHLUNGSWEISEID   PIC S9(4) COMP.
             10 PO-DRKI1097-VERDATPRODLOESCH     PIC X(10).




      *
      ******** Ende Copy-Member **************************************

