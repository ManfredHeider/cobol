      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.04.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2034.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2034.CPv  $
      *
      *   Rev 2.3   Apr 04 2017 14:23:14   t13ac20
      *AKI01503: OZPs bereinigen
      *
      *   Rev 2.2   02 Feb 2009 10:18:36   t13ex27
      *AKI00768: CR 22257 - Bearbeitungskennzeichen Rechenwerte-DB-
      *Änderung
      *
      *   Rev 2.1   25 Apr 2007 15:35:28   t13ex43
      *AKI00556: List-OZPs für LEBENFOND/-TEILLEISTUNG
      *
      *   Rev 2.0   25 Apr 2007 13:39:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2034                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LIST-OZP ZXKI0329                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    25.04.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * AKI00556!25.04.07!2.1 ! NEU                       ! KLEEMEIER *
      * AKI01503!31.03.17!2.3 ! FELDER ENTFALLEN:         ! SWO       *
      *         !        !    !   - NI-LTLAUSZBETRAGWK    !           *
      *         !        !    !   - LTLAUSZBETRAGWKZ      !           *
      *         !        !    !   - NI-LTLAUSZDECKKAPW    !           *
      *         !        !    !   - LTLAUSZDECKKAPWKZ     !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI2034 - LESEN TABELLE LEBENTEILLEISTUNG        *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI2034.
           05  PI-DRKI2034-UFKT-CODE        PIC  9(002).
           05  PI-DRKI2034-LTLVERTRAGSID    PIC  X(013).
      *
       01  PO-DRKI2034.
           05  PO-DRKI2034-RC               PIC  X(003).
               88 PO-DRKI2034-RC-OK              VALUE 'OK '.
               88 PO-DRKI2034-RC-EOF             VALUE 'EOF'.
               88 PO-DRKI2034-RC-NOT-FOUND       VALUE 'NFD'.
           05  PO-DRKI2034-NI.
               10 PO-DRKI2034-NI-LTLVERTRAGSID   PIC S9(04) COMP.
               10 PO-DRKI2034-NI-LTLVERSTARLFDNR PIC S9(04) COMP.
               10 PO-DRKI2034-NI-LTLLAUFENDENR   PIC S9(04) COMP.
               10 PO-DRKI2034-NI-LTLAUSZAHLBETRA PIC S9(04) COMP.
               10 PO-DRKI2034-NI-LTLAUSZAHLZEITP PIC S9(04) COMP.
               10 PO-DRKI2034-NI-LETZTEAENDERUNG PIC S9(04) COMP.
               10 PO-DRKI2034-NI-LTLVERRENTKAPBE PIC S9(04) COMP.
               10 PO-DRKI2034-NI-LTLVERRENTKAPBW PIC S9(04) COMP.
               10 PO-DRKI2034-NI-LTLRENTEGARZEIT PIC S9(04) COMP.
               10 PO-DRKI2034-NI-LTLAUSZDECKKAPB PIC S9(04) COMP.
               10 PO-DRKI2034-NI-LTLAUSZBETRAGBK PIC S9(04) COMP.
               10 PO-DRKI2034-NI-LTLZAHLZEITPBKZ PIC S9(04) COMP.
               10 PO-DRKI2034-NI-LTLVERRENTKAPBK PIC S9(04) COMP.
           05  PO-DRKI2034-DCL.
               10 PO-DRKI2034-LTLVERTRAGSID      PIC  X(013).
               10 PO-DRKI2034-LTLVERSTARLFDNR    PIC S9(04) COMP.
               10 PO-DRKI2034-LTLLAUFENDENR      PIC S9(04) COMP.
               10 PO-DRKI2034-LTLAUSZAHLBETRAG   PIC S9(09)V9(2) COMP-3.
               10 PO-DRKI2034-LTLAUSZAHLZEITPKT  PIC  X(010).
               10 PO-DRKI2034-LETZTEAENDERUNG    PIC  X(026).
               10 PO-DRKI2034-LTLVERRENTKAPBETR  PIC S9(09)V9(2) COMP-3.
               10 PO-DRKI2034-LTLVERRENTKAPBWKZ  PIC  X(001).
               10 PO-DRKI2034-LTLRENTEGARZEIT    PIC S9(04) COMP.
               10 PO-DRKI2034-LTLAUSZDECKKAPBETR PIC S9(09)V9(2) COMP-3.
               10 PO-DRKI2034-LTLAUSZBETRAGBKZ   PIC X(1).
               10 PO-DRKI2034-LTLZAHLZEITPBKZ    PIC X(1).
               10 PO-DRKI2034-LTLVERRENTKAPBKZ   PIC X(1).
      ****
      **** ENDE COPY DRKI2034 ****************************************
      ****

