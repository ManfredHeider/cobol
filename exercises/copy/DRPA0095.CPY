      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0095.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0095.CPv  $
      *
      *   Rev 2.6   05 Mar 2018 11:35:42   t13ex67
      *APA02481: Anzeige des Buchungsdatums im PG11 - MSS
      *
      *   Rev 2.5   Jul 16 2015 15:06:42   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.4   Jun 19 2012 09:22:22   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:47:22   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 08:21:16   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   Apr 19 2006 09:45:36   t13ae46
      *APA00449: Kontrolle manueller Datensätze (Anforderung KPMG a
      *n Daisy)
      *
      *   Rev 2.0   Feb 27 1999 14:27:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   11 Nov 1998 09:01:38   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   Mar 06 1997 14:57:30   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0095  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRÖMPELER                                       *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !20.08.96! 01 ! NEUERSTELLUNG             ! PRÖMPELER*
      *         !08.12.97! 1.1! ANPASSUNG PDTA            ! D.WILHELM*
      *         !05.04.06! 1.2! NEUE FELDER               ! WANG     *
      * APA00622!23.05.07! 2.2! NEU: KNR-VGES             ! STORCH   *
      * APA01101!23.02.12! 2.3! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01932!22.04.15! 2.5! PDTA-UST-BETRAG HINZU     ! WANG     *
      * APA02481!10.07.17! 2.6! PDTA-BUCH-DAT HINZU       ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0095                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       PDTA TBPA0017                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0095.
          05 PI-DRPA0095-LAST-UPDATE          PIC  X(026).
          05 PI-DRPA0095-PDTA-KEY.
             10 PI-DRPA0095-PDTA-ID           PIC S9(015)
                                              PACKED-DECIMAL.
          05 PI-DRPA0095-PDTA-DATEN.
             10 PI-DRPA0095-PGEB-GES          PIC S9(004) BINARY.
             10 PI-DRPA0095-KNR-DTA-ART       PIC S9(004) BINARY.
             10 PI-DRPA0095-KNR-DTA-SPLIT-ART PIC S9(004) BINARY.
             10 PI-DRPA0095-PDTA-ORIGINAL-ID  PIC S9(015)V COMP-3.
             10 PI-DRPA0095-PDTA-ERSTELL-DAT  PIC  X(010).
             10 PI-DRPA0095-PDTA-TRANSF-DAT   PIC  X(010).
             10 PI-DRPA0095-PDTA-STAND-DAT    PIC  X(010).
             10 PI-DRPA0095-PDTA-ANZ-SATZ     PIC S9(009) BINARY.
             10 PI-DRPA0095-PDTA-ANZ-FEHL     PIC S9(009) BINARY.
             10 PI-DRPA0095-PDTA-KONTR-WERT   PIC S9(011)V99
                                              PACKED-DECIMAL.
             10 PI-DRPA0095-PDTA-INHALT       PIC  X(040).
             10 PI-DRPA0095-PDTA-VSTAT        PIC  X(004).
             10 PI-DRPA0095-PDTA-ERSTELLT-VON PIC  X(008).
             10 PI-DRPA0095-PDTA-PVW-VON      PIC  X(008).
             10 PI-DRPA0095-PDTA-PVW-DAT      PIC  X(010).
             10 PI-DRPA0095-KNR-MANDANT       PIC S9(004) BINARY.
220415       10 PI-DRPA0095-PDTA-UST-BETRAG   PIC S9(009)V9(2)
220415                                        PACKED-DECIMAL.
100717       10 PI-DRPA0095-PDTA-BUCH-DAT     PIC  X(010).
      *
       01 PO-DRPA0095.
          05 PO-DRPA0095-RC                   PIC  X(003).
             88 PO-DRPA0095-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0095-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0095-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****************************************************************


