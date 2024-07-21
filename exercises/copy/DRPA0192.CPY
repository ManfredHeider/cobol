      * T13EX13   Stand: 20/01/97    16:16:36
      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  Januar 1997
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0192.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0192.CPv  $
      *
      *   Rev 2.4   Jun 03 2019 09:50:10   t13ae46
      *FPA01690: Ges. 42 Prod. 22 
      *
      *   Rev 2.3   May 17 2019 09:34:30   t13ae46
      *FPA01690: Ges. 42 Prod. 22
      *
      *   Rev 2.2   Mar 28 2019 09:42:26   t13ae46
      *FPA01690: Ges. 42 Prod. 22
      *
      *   Rev 2.1   Mar 27 2019 12:42:50   t13ae46
      *FPA01690: Ges. 42 Prod. 22
      *
      *   Rev 2.0   Feb 27 1999 14:28:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   26 Jun 1997 14:45:24   T13EX21
      *DAISY Release 1.0.3
      *
      *   Rev 1.1   Apr 09 1997 16:50:22   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:07:50   P13PVCS
      *Initial revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0192  FUNKTION PE - STRUK ERMITTELN      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: GIESE                                           *
      *    DATUM:    20.01.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !20.01.97! 01 ! NEUERSTELLUNG             ! GIESE    *
      * --------!--------!----!---------------------------!--------  *
      * AFPA0010!19.06.97! 02 ! ALLE KLEINBUCHSTABEN IN   ! SCHäFER  *
      *         !        !    ! GROSSBUCHSTABEN UMGEWAND.,!          *
      *         !        !    ! WEIL VALUE-WERT FALSCH WAR!          *
      * --------!--------!----!---------------------------!--------  *
      * FPA01690!11.02.19!2.1 ! PO-DRPA0192-RC-UNZ  UND   ! WANG     *
      *         !        !    ! PI-DRPA0192-MANDANT NEU   !          *
      *         !        !2.2 ! ÄNDERUNGEN 2.1 ENTFERNEN  ! WANG     *
      *         !        !2.3 ! 2.1 AKTIVIEREN            ! WANG     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0192                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *                                                              *
      *  UNTERFUNKTION-1: AUS ALTANWENDUNG / DEFAULT                 *
      *  UNTERFUNKTION-2: AUS DAISY                                  *
      *                                                              *
      *                                                              *
       01 PI-DRPA0192.
          05 PI-DRPA0192-UFUNKTION              PIC  X(01).
             88 PI-DRPA0192-UFKT-1       VALUE '1'.
             88 PI-DRPA0192-UFKT-2       VALUE '2'.
          05 PI-DRPA0192-DATEN-UFKT-1.
      ***    AUFRUF AUS ALTANWENDUNG
             10 LS-VBNR1                        PIC S9(09) BINARY.
             10 LS-VBNR2                        PIC S9(09) BINARY.
             10 LS-DAT                          PIC  9(06).
          05 PI-DRPA0192-DATEN-UFKT-2.
             10 PI-DRPA0192-AVERM               PIC S9(09) BINARY.
             10 PI-DRPA0192-PGEB-GES            PIC S9(04) BINARY.
             10 PI-DRPA0192-STICHTAG            PIC  X(10).
110219       10 PI-DRPA0192-MANDANT             PIC S9(04) BINARY.

       01 PO-DRPA0192.
          05 PO-DRPA0192-RC                     PIC  X(003).
             88 PO-DRPA0192-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0192-RC-NOK                   VALUE 'NOK'.
110219       88 PO-DRPA0192-RC-UNZ                   VALUE 'UNZ'.
          05 PO-AG8-TAB.
             10 LS-AG8-DATEN      OCCURS 18.
               15  LS-AG8VBNR      PIC 9(7)    COMP-3.
               15  LS-AG8PZ        PIC X.
               15  LS-AG8NAME      PIC X(20).
               15  LS-AG8STRUK     PIC 999     COMP-3.
               15  LS-AG8STRUKT    PIC X(4).
               15  LS-AG8PROV      PIC 999V9   COMP-3.
             10  LS-FKZ               PIC 99.
             10  LS-DIR               PIC 999     COMP-3.
          05 PO-DRPA0192-DATEN.
             10 PO-DRPA0192-PE-STRUKTUR OCCURS 20.
               15  PO-DRPA0192-PE-NAME            PIC X(20).
               15  PO-DRPA0192-EMP-TYP-BEZ        PIC X(10).
             10 PO-DRPA0192-VTAB-MAX            PIC  9(002).
             10 PO-DRPA0192-VSTAT-TABELLE.
                15 PO-DRPA0192-VSTAT-TAB        OCCURS 20
                                       INDEXED BY PO-DRPA0192-VTAB-IND.
                   20 PO-DRPA0192-VTAB-VSTAT    PIC  X(004).
                   20 PO-DRPA0192-VTAB-VSTAT-UNR PIC S9(004) BINARY.
                   20 PO-DRPA0192-VTAB-ORT      PIC  X(030).
                   20 PO-DRPA0192-VTAB-BEREICH  PIC  X(010).
             10 PO-DRPA0192-AVERM               PIC S9(009) BINARY.
             10 PO-DRPA0192-VM                  PIC S9(009) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************




