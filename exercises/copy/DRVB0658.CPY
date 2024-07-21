      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0658.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0658.CPv  $
      *
      *   Rev 2.1   Jun 10 2003 14:21:48   t13ex22
      *AVB00666: Einheitenberechnung Basis-/Steigerungszeitraum
      *
      *   Rev 2.0   Feb 17 2003 10:57:54   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0658 FUNKTION FÜR UPVB0130
      *    --------------------------------------------------------    *
      *    FUNKTION VERGLEICHT BERECHNET EINHEITEN IM BASIS- UND IM
      *    STEIGERUNGSZEITRAUM. HERAUSGEWACHSENE DIREKTIONEN WERDEN IM
      *    BASISZEITRAUM SO BEHANDELT, ALS WÄREN SIE NIE ENTHALTEN
      *    GEWESEN. IM STEIGERUNGSZEITRAUM ERFOLGT DIE BERECHNUNG ALS
      *    WÄREN DIE DIREKTIONEN VOLLSTÄNDIG ENTHALTEN.
      *
      *    ACHTUNG: DIESE FUNKTION FÜHRT AUF MONATSEBENE KEINE NULL-
      *    SETZUNG NEGATIVER DIFFERENZEN DURCH.D.H., WENN DIE DIFFERENZ
      *    DER EINHEITEN ZWISCHEN ALTER UND NEUER DIREKTION NEGATIV
      *    WÜRDE, SO WÜRDE DIES IN DER BERECHNUNG AUCH SO WEITEREGGEBEN!
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    22.05.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00666!22.05.03! 01 ! NEU                       ! MUND     *
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
       01  PI-DRVB0658.
           05  PI-DRVB0658-DATEN.
               10 PI-DRVB0658-MA-ST-NR         PIC S9(009) COMP.
               10 PI-DRVB0658-BASIS-VON        PIC  X(010).
               10 PI-DRVB0658-BASIS-BIS        PIC  X(010).
               10 PI-DRVB0658-BASIS-ENDE       PIC  X(010).
               10 PI-DRVB0658-STEIGERUNG-VON   PIC  X(010).
               10 PI-DRVB0658-STEIGERUNG-BIS   PIC  X(010).
      *
       01  PO-DRVB0658.
           05  PO-DRVB0658-RC                  PIC  X(003).
               88 PO-DRVB0658-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0658-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0658-MA-ST-NR            PIC S9(009) COMP.
           05  PO-DRVB0658-EH-BASIS-AD         PIC S9(09)V99 COMP.
           05  PO-DRVB0658-EH-BASIS-ND         PIC S9(09)V99 COMP.
           05  PO-DRVB0658-EH-STEIGERUNG-AD    PIC S9(09)V99 COMP.
           05  PO-DRVB0658-EH-STEIGERUNG-ND    PIC S9(09)V99 COMP.
           05  PO-DRVB0658-NEUE-DIREKTIONEN    PIC  9(001).
               88 PO-DRVB0658-ND-VORHANDEN                 VALUE 1.
               88 PO-DRVB0658-ND-NICHT-VORHANDEN           VALUE 2.

