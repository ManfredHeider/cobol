      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  12.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3803.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3803.CPv  $
      *
      *   Rev 2.1   Jun 22 2020 11:12:00   t13ex78
      *AKI01780: List OPZ G90EOSVERTRAG
      *
      *   Rev 2.0   Jun 12 2020 10:45:28   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3803                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0538 G66_PRIVATKREDIT               *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01780!12.06.20!2.0 ! Neuerstellung             ! t13ex78   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3803
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
      *
       01  PI-DRKI3803.
           05 PI-DRKI3803-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3803-UFKT-FIRST        VALUE 1.
              88 PI-DRKI3803-UFKT-NEXT         VALUE 2.
              88 PI-DRKI3803-UFKT-QUIT         VALUE 3.
 
           05  PI-DRKI3803-KEY.
               10 PI-DRKI3803-G90EMANDANT      PIC S9(9) COMP.
 
      *
       01  PO-DRKI3803.
           05 PO-DRKI3803-RC                   PIC  X(003).
              88 PO-DRKI3803-RC-OK             VALUE 'OK '.
              88 PO-DRKI3803-RC-LEER           VALUE 'LER'.
              88 PO-DRKI3803-RC-EOF            VALUE 'EOF'.
      *
           05 PO-DRKI3803-DATEN.
              10 PO-DRKI3803-G90EVERTRAGSNR    PIC X(15).
              10 PO-DRKI3803-G90EMANDANT       PIC S9(9) COMP.
              10 PO-DRKI3803-G90EZUSTAND       PIC X(1).
              10 PO-DRKI3803-G90EABSCHLVBNR    PIC S9(9) COMP.
              10 PO-DRKI3803-G90EPRODUKTART    PIC S9(1)V9(01) COMP-3.
              10 PO-DRKI3803-G90EGEBNICHTANG   PIC S9(1)V9(2) COMP-3.
              10 PO-DRKI3803-G90EFACTGEBPROZ   PIC S9(1)V9(2) COMP-3.
              10 PO-DRKI3803-G90EFACTANKRAHMEN PIC S9(10)V9(2) COMP-3.
              10 PO-DRKI3803-G90EVERTRAGBEGINN PIC X(8).
              10 PO-DRKI3803-G90EVERTRAGENDE   PIC X(8).
              10 PO-DRKI3803-G90EEINRICHTGEB   PIC S9(8)V9(2) COMP-3.
              10 PO-DRKI3803-G90EIBAN          PIC X(34).
              10 PO-DRKI3803-G90EINFOSPERREKZN PIC X(1).
              10 PO-DRKI3803-G90EVONDATJAHR    PIC X(8).
              10 PO-DRKI3803-G90EBISDATJAHR    PIC X(8).
              10 PO-DRKI3803-G90ERECHNUNGJAHR  PIC S9(8)V9(01) COMP-3.
              10 PO-DRKI3803-G90EUMSATZJAHR    PIC S9(8)V9(2) COMP-3.
              10 PO-DRKI3803-G90EVONDATMONAT   PIC X(8).
              10 PO-DRKI3803-G90EBISDATMONAT   PIC X(8).
              10 PO-DRKI3803-G90ERECHNUNGMONAT PIC S9(8)V COMP-3.
              10 PO-DRKI3803-G90EUMSATZMONAT   PIC S9(8)V9(2) COMP-3.
 

 
      ******* ENDE COPY-MEMBER ***************************************



