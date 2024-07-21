      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0698.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0698.CPv  $
      *
      *   Rev 2.1   Jan 28 2004 10:58:58   t13ex22
      *AAS00443: WBW2004 erm. ND über gesamten Basiszeitraum
      *
      *   Rev 2.0   Feb 17 2003 11:01:50   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0698  FUNKTION ZU UPVB0130
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
      * AAS00443!28.01.04! 01 ! NEU                       ! MUND     *
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
       01  PI-DRVB0698.
           05  PI-DRVB0698-DATEN.
               10 PI-DRVB0698-MA-ST-NR         PIC S9(009) COMP.
               10 PI-DRVB0698-BASIS-VON        PIC  X(010).
               10 PI-DRVB0698-BASIS-BIS        PIC  X(010).
               10 PI-DRVB0698-BASIS-ENDE       PIC  X(010).
               10 PI-DRVB0698-STEIGERUNG-VON   PIC  X(010).
               10 PI-DRVB0698-STEIGERUNG-BIS   PIC  X(010).
      *
       01  PO-DRVB0698.
           05  PO-DRVB0698-RC                  PIC  X(003).
               88 PO-DRVB0698-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0698-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0698-MA-ST-NR            PIC S9(009) COMP.
           05  PO-DRVB0698-EH-BASIS            PIC S9(09)V99 COMP.
           05  PO-DRVB0698-EH-STEIGERUNG       PIC S9(09)V99 COMP.
           05  PO-DRVB0698-NEUE-DIREKTIONEN    PIC  9(001).
               88 PO-DRVB0698-ND-VORHANDEN                 VALUE 1.
               88 PO-DRVB0698-ND-NICHT-VORHANDEN           VALUE 2.


