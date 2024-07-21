      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.01.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3054.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3054.CPv  $
      *
      *   Rev 2.1   Jan 11 2016 16:57:46   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - Li
      *st-OZP für TCKI0084
      *
      *   Rev 2.0   Jan 07 2016 08:06:22   t13ex78
      *Archiverstellung
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3054 FUNKTION:KUEBLACKLIS
      *                                   LIST KUBPERSONENID
      *    -------------------------------------------------------
      *    ERSTELLT: BRENNECKE
      *    DATUM:
      *****************************************************************
      *    VERSION   01
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * ---------------------------------------------------------------
      * AKI01382!07.01.16!    ! Neuerstellung             ! tfl
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3054
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: KUNDENUEBERTRAGUNG: LIST
      *
      *****************************************************************
       01 PI-DRKI3054.
           05  PI-DRKI3054-UFKT                PIC  9(002).
               88 PI-DRKI3054-UFKT-FIRST       VALUE 1.
               88 PI-DRKI3054-UFKT-NEXT        VALUE 2.
               88 PI-DRKI3054-UFKT-QUIT        VALUE 3.
           05  PI-DRKI3054-DATEN.
               10 PI-DRKI3054-KUBPERSONENID    PIC X(13).
 
       01 PO-DRKI3054.
          05 PO-DRKI3054-RC                    PIC  X(003).
             88 PO-DRKI3054-RC-OK              VALUE 'OK '.
             88 PO-DRKI3054-RC-LEER            VALUE 'LER'.
             88 PO-DRKI3054-RC-EOF             VALUE 'EOF'.
          05 PO-DRKI0588-DATEN.
             10 PO-DRKI3054-KUBPERSONENID      PIC  X(013).
             10 PO-DRKI3054-KUBVBID            PIC  X(013).
             10 PO-DRKI3054-KUBERLEDIGTKZ      PIC  X(001).
             10 PO-DRKI3054-KUBTICKETID        PIC S9(18)V USAGE COMP-3.
             10 PO-DRKI3054-LETZTEAENDERUNG    PIC  X(026).
 
      ******** Ende Copy-Member **************************************

