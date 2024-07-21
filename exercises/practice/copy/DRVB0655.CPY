      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0655.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0655.CPv  $
      *
      *   Rev 2.1   Jun 10 2003 14:21:18   t13ex22
      *AVB00666: Einheitenberechnung Basis-/Steigerungszeitraum
      *
      *   Rev 2.0   Feb 17 2003 10:57:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0655  FUNKTION ZU UPVB0130
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
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    22.04.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00666!22.04.03! 01 ! NEU                       ! MUND     *
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
       01  PI-DRVB0655.
           05  PI-DRVB0655-DATEN.
               10 PI-DRVB0655-MA-ST-NR         PIC S9(009) COMP.
               10 PI-DRVB0655-BASIS-VON        PIC  X(010).
               10 PI-DRVB0655-BASIS-BIS        PIC  X(010).
               10 PI-DRVB0655-BASIS-ENDE       PIC  X(010).
               10 PI-DRVB0655-STEIGERUNG-VON   PIC  X(010).
               10 PI-DRVB0655-STEIGERUNG-BIS   PIC  X(010).
      *
       01  PO-DRVB0655.
           05  PO-DRVB0655-RC                  PIC  X(003).
               88 PO-DRVB0655-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0655-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0655-MA-ST-NR            PIC S9(009) COMP.
           05  PO-DRVB0655-EH-BASIS            PIC S9(09)V99 COMP.
           05  PO-DRVB0655-EH-STEIGERUNG       PIC S9(09)V99 COMP.
           05  PO-DRVB0655-NEUE-DIREKTIONEN    PIC  9(001).
               88 PO-DRVB0655-ND-VORHANDEN                 VALUE 1.
               88 PO-DRVB0655-ND-NICHT-VORHANDEN           VALUE 2.


