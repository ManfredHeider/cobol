      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.10.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2700.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2700.CPv  $
      *
      *   Rev 2.3   Jan 31 2018 11:28:44   t13ex78
      *AKI01553: Abgleich bei KÜ: mehrer Steuersätze wenn mehrere Q
      *uell-VBs
      *
      *   Rev 2.2   Mar 21 2016 16:39:16   t13ex27
      *FKI01156: Wartungsauftrag Abgleich
      *
      *   Rev 2.1   Nov 12 2015 13:16:52   t13ex27
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect.-Abgl
      *eich
      *
      *   Rev 2.0   Oct 18 2012 12:07:54   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2700                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2700                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01382!27.10.15!2.1 ! neu                       ! I.Frenzel*
      * FKI01156!17.02.16!2.2 ! DBO-1464 abweichender VB  ! I.Frenzel*
011117* AKI01553!01.11.17!.157! Bei neuen KÜ-Tickets (972 ! tfl      *
      *         !        !    ! und 979) kann es mehrere  !          *
      *         !        !    ! Quell-VBs geben. Dann muss!          *
      *         !        !    ! für jeden ein Steuersatz  !          *
      *         !        !    ! KUNDENUEBERTRAGUNG V2 OK  !          *
      *         !        !    ! Der Aufbau der Ticketwerte!          *
      *         !        !    ! läßt theoretisch 9999 ver-!          *
      *         !        !    ! schiedene VBs zu; mehr als!          *
      *         !        !    ! 5 werden es aber in der   !          *
      *         !        !    ! Realität wohl nie         !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2700
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    Steuersatz für Kundenübertragung Version 2
      *
      ****************************************************************
      *
       01  PI-DRKI2700.
 
           05  PI-DRKI2700-DATEN.
 
             10 PI-DRKI2700-OTHOSTKUNDENID        PIC X(13).
             10 PI-DRKI2700-OTTICKETID            PIC S9(018) COMP-3.
 
       01  PO-DRKI2700.
           05  PO-DRKI2700-RC                     PIC  X(003).
               88 PO-DRKI2700-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2700-RC-NOT-FOUND           VALUE 'NFD'.
               88 PO-DRKI2700-RC-NOK                 VALUE 'NOK'.
 
           05  PO-DRKI2700-DATEN.
 
170216*        10  PO-DRKI2700-KUNDENDATEN        PIC  X(098).
011117         10  PO-DRKI2700-KUNDENDATEN PIC X(098) OCCURS 9999.
011117         10  PO-DRKI2700-IND         PIC S9(008) COMP.
 
           05  PO-DRKI2700-NFD-DATEN.
               10  PO-DRKI2700-TABELLE            PIC  X(030).
               10  PO-DRKI2700-TABELLE-ID         PIC  X(013).
 
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

