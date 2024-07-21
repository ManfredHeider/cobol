      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.10.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0799.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0799.CPv  $
      *
      *   Rev 2.1   Nov 04 2005 11:57:06   T13AE27
      *AAS00572: Siegeradler 2005
      *
      *   Rev 2.0   Oct 12 2005 11:19:20   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0799  FUNKTION ZU UPVB0130
      *    --------------------------------------------------------   *
      *    FUNKTION VERGLEICHT BERECHNET EINHEITEN IM BASIS- UND IM
      *    STEIGERUNGSZEITRAUM. HERAUSGEWACHSENE DIREKTIONEN WERDEN IM
      *    BASISZEITRAUM SO BEHANDELT, ALS WÄREN SIE NIE ENTHALTEN
      *    GEWESEN. IM STEIGERUNGSZEITRAUM ERFOLGT DIE BERECHNUNG ALS
      *    WÄREN DIE DIREKTIONEN VOLLSTÄNDIG ENTHALTEN.
      *
      *    ACHTUNG: DIESE FUNKTION FÜHRT AUF MONATSEBENE EINE NULL-
      *    SETZUNG NEGATIVER DIFFERENZEN DURCH.D.H., WENN DIE DIFFERENZ
      *    DER EINHEITEN ZWISCHEN ALTER UND NEUER DIREKTION NEGATIV
      *    WÜRDE, SO WÜRDE DIES ALS NULL BETRACHTET.
      *
      *
      *    FUNKTION ENTSPRICHT DRVB0655 BIS AUF FOLGENDE ÄNDERUNG:
      *    BEI HERAUSGEWACHSENEN DIREKTIONEN WIRD ZUNÄCHST DER GESAMTE
      *    BASISZEITRAUM GEPRÜFT. AUCH WENN EINE DIREKTION ERST IM Z.B.
      *    MAI HERAUSGEWACHSEN IST, WERDEN DIE EINHEITEN BEREITS BEI DER
      *    ERMITTLUNG DES MONATS MÄRZ HERAUSGERECHNET.
      *    DIESE ART DER BERECHNUNG IST NUR VON RELEVANZ, WENN BASIS-
      *    UND STEIGERUNGSZEITRAUM NICHT KOMPLETT, SONDERN MONATSBEZOGEN
      *    ABGEFRAGET WERDEN.
      *
      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    22.04.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00572!12.10.05! 01 ! NEU                       ! Duerr    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-DRVB0799.
           05  PI-DRVB0799-DATEN.
               10 PI-DRVB0799-MA-ST-NR         PIC S9(009) COMP.
               10 PI-DRVB0799-BASIS-VON        PIC  X(010).
               10 PI-DRVB0799-BASIS-BIS        PIC  X(010).
               10 PI-DRVB0799-BASIS-ENDE       PIC  X(010).
               10 PI-DRVB0799-STEIGERUNG-VON   PIC  X(010).
               10 PI-DRVB0799-STEIGERUNG-BIS   PIC  X(010).
      *
       01  PO-DRVB0799.
           05  PO-DRVB0799-RC                  PIC  X(003).
               88 PO-DRVB0799-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0799-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0799-MA-ST-NR            PIC S9(009) COMP.
           05  PO-DRVB0799-EH-BASIS            PIC S9(09)V99 COMP.
           05  PO-DRVB0799-EH-STEIGERUNG       PIC S9(09)V99 COMP.
           05  PO-DRVB0799-NEUE-DIREKTIONEN    PIC  9(001).
               88 PO-DRVB0799-ND-VORHANDEN                 VALUE 1.
               88 PO-DRVB0799-ND-NICHT-VORHANDEN           VALUE 2.


