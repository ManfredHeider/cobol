      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.04.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2032.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2032.CPv  $
      *
      *   Rev 2.4   02 Feb 2009 10:18:32   t13ex27
      *AKI00768: CR 22257 - Bearbeitungskennzeichen Rechenwerte-DB-
      *Änderung
      *
      *   Rev 2.3   Jul 21 2008 09:55:46   t13ae49
      *AKI00711: Abgleich: Änderung Rechenwertaktualisierung
      *
      *   Rev 2.2   09 Jun 2008 15:58:38   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben OZPs
      *
      *   Rev 2.1   25 Apr 2007 15:34:56   t13ex43
      *AKI00556: List-OZPs für LEBENFOND/-TEILLEISTUNG
      *
      *   Rev 2.0   25 Apr 2007 13:38:50   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2032                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LIST-OZP ZXKI0327                    *
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
      * AKI00711!10.04.08!2.2 ! Neues Attribut            ! I. Frenzel*
100708* AKI00711!10.07.08!2.3 ! 'PO-DRKI2032-' WIEDER VOR ! JWH       *
100708*         !        !    ! DIE 2 VAR. GESTELLT       !           *
      * DBO-9909!29.07.22!    ! Feld LFOFONDISIN gelöscht ! u131848   *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI2032 - LESEN TABELLE LEBENFOND                *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI2032.
           05  PI-DRKI2032-UFKT-CODE        PIC  9(002).
           05  PI-DRKI2032-LFOVERTRAGSID    PIC  X(013).
      *
       01  PO-DRKI2032.
           05  PO-DRKI2032-RC               PIC  X(003).
               88 PO-DRKI2032-RC-OK              VALUE 'OK '.
               88 PO-DRKI2032-RC-EOF             VALUE 'EOF'.
               88 PO-DRKI2032-RC-NOT-FOUND       VALUE 'NFD'.
           05  PO-DRKI2032-NI.
               10 PO-DRKI2032-NI-LFOVERTRAGSID    PIC S9(04) COMP.
               10 PO-DRKI2032-NI-LFOVERSTARLFDNR  PIC S9(04) COMP.
               10 PO-DRKI2032-NI-LFOLFDNR         PIC S9(04) COMP.
               10 PO-DRKI2032-NI-LFOANZAHLANTEIL  PIC S9(04) COMP.
               10 PO-DRKI2032-NI-LFORUECKNAHMEBE  PIC S9(04) COMP.
               10 PO-DRKI2032-NI-LFOPROZSPARBETR  PIC S9(04) COMP.
               10 PO-DRKI2032-NI-LFODECKUNGSKAPI  PIC S9(04) COMP.
               10 PO-DRKI2032-NI-LFODECKKAPITWKZ  PIC S9(04) COMP.
               10 PO-DRKI2032-NI-LETZTEAENDERUNG  PIC S9(04) COMP.
100408         10 PO-DRKI2032-NI-LFOFONDSKUERZ    PIC S9(04) COMP.
291208         10 PO-DRKI2032-NI-LFOANZAHLANTBKZ  PIC S9(04) COMP.
291208         10 PO-DRKI2032-NI-LFORUECKNAHMEBK  PIC S9(04) COMP.
291208         10 PO-DRKI2032-NI-LFOPROZSPARBKZ   PIC S9(04) COMP.
291208         10 PO-DRKI2032-NI-LFODECKKAPITBKZ  PIC S9(04) COMP.
291208         10 PO-DRKI2032-NI-LFOFONDSKUEBKZ   PIC S9(04) COMP.
           05  PO-DRKI2032-DCL.
               10 PO-DRKI2032-LFOVERTRAGSID      PIC  X(013).
               10 PO-DRKI2032-LFOVERSTARLFDNR    PIC S9(04) COMP.
               10 PO-DRKI2032-LFOLFDNR           PIC S9(04) COMP.
100708         10 PO-DRKI2032-LFOANZAHLANTEILE   PIC S9(8)V9(6) COMP-3.
100708         10 PO-DRKI2032-LFORUECKNAHMEBETR  PIC S9(11)V9(2) COMP-3.
               10 PO-DRKI2032-LFOPROZSPARBETR    PIC S9(03)V9(2) COMP-3.
               10 PO-DRKI2032-LFODECKUNGSKAPITAL PIC S9(11)V9(2) COMP-3.
               10 PO-DRKI2032-LFODECKKAPITWKZ    PIC  X(001).
               10 PO-DRKI2032-LETZTEAENDERUNG    PIC  X(026).
100408         10 PO-DRKI2032-LFOFONDSKUERZ      PIC  X(002).
291208         10 PO-DRKI2032-LFOANZAHLANTBKZ    PIC  X(001).
291208         10 PO-DRKI2032-LFORUECKNAHMEBKZ   PIC  X(001).
291208         10 PO-DRKI2032-LFOPROZSPARBKZ     PIC  X(001).
291208         10 PO-DRKI2032-LFODECKKAPITBKZ    PIC  X(001).
291208         10 PO-DRKI2032-LFOFONDSKUEBKZ     PIC  X(001).
      ****
      **** ENDE COPY DRKI2032 ****************************************
      ****
