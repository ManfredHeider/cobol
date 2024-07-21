      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0094.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRPA0094.CPv  $
      *
      *   Rev 2.8   05 Mar 2018 11:35:28   t13ex67
      *APA02481: Anzeige des Buchungsdatums im PG11 - MSS
      *
      *   Rev 2.7   Jul 16 2015 15:06:32   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.6   Jun 19 2012 09:22:14   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.5   May 21 2012 08:47:14   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.4   Dec 10 2007 08:16:56   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.3   Apr 19 2006 09:46:18   t13ae46
      *APA00449: Kontrolle manueller Datensätze (Anforderung KPMG a
      *n Daisy)
      *
      *   Rev 2.2   12 Sep 2001 14:31:28   t13ae46
      *APA00144: LAST-UPDATE,KENNUNG ergaenzt
      *
      *   Rev 2.1   Aug 23 1999 11:33:00   T13EX21
      *APA00049 - Unterfunktion eingefügt
      *
      *   Rev 2.0   Feb 27 1999 14:27:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   11 Nov 1998 09:01:06   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   Mar 06 1997 14:57:24   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0094  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Prömpeler                                       *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !20.08.96! 01 ! NEUERSTELLUNG             ! PRÖMPELER*
      *         !10.02.97!    ! ANPASSUNG PDTA            ! GIESE    *
      *         !08.12.97!    ! ANPASSUNG PDTA            ! D.WILHELM*
      * APA00049!02.08.99! 02 ! UFKT. EINGEFÜGT           ! STORCH   *
      * APA00144!09.08.01! 03 ! PI-DRPA0094-PDTA-LAST-    ! WANG     *
      *         !        !    ! UPDATE,PI-DRPA0094-KENNUNG!          *
      *         !        !    ! ERGÄNZT                   !          *
      * APA00449!05.04.06! 2.3! NEUE FELDER HINZU         ! WANG     *
      * APA00622!23.05.07! 2.4! KNR-VGES HINZU (ALLFINANZ)! STORCH   *
      * APA01101!23.02.12! 2.5! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01932!22.04.15! 2.6! PDTA-UST-BETRAG HINZU     ! WANG     *
      * APA02481!10.07.17! 2.7! PDTA-BUCH-DAT HINZU       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0094                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PDTA TBPA0017              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0094.
          05 PI-DRPA0094-UFKT                  PIC  X(002).
          05 PI-DRPA0094-PDTA-KEY.
             10 PI-DRPA0094-PDTA-ID           PIC S9(015)
                                              PACKED-DECIMAL.
          05 PI-DRPA0094-PDTA-DATEN.
             10 PI-DRPA0094-PGEB-GES          PIC S9(004) BINARY.
             10 PI-DRPA0094-KNR-DTA-ART       PIC S9(004) BINARY.
             10 PI-DRPA0094-KNR-DTA-SPLIT-ART PIC S9(004) BINARY.
             10 PI-DRPA0094-PDTA-ORIGINAL-ID  PIC S9(015)V COMP-3.
             10 PI-DRPA0094-PDTA-ERSTELL-DAT  PIC  X(010).
             10 PI-DRPA0094-PDTA-TRANSF-DAT   PIC  X(010).
             10 PI-DRPA0094-PDTA-STAND-DAT    PIC  X(010).
             10 PI-DRPA0094-PDTA-ANZ-SATZ     PIC S9(009) BINARY.
             10 PI-DRPA0094-PDTA-ANZ-FEHL     PIC S9(009) BINARY.
             10 PI-DRPA0094-PDTA-KONTR-WERT   PIC S9(011)V9(2)
                                              PACKED-DECIMAL.
             10 PI-DRPA0094-PDTA-INHALT       PIC  X(040).
             10 PI-DRPA0094-PDTA-VSTAT        PIC  X(004).
             10 PI-DRPA0094-PDTA-ERSTELLT-VON PIC  X(008).
             10 PI-DRPA0094-PDTA-PVW-VON      PIC  X(008).
             10 PI-DRPA0094-PDTA-PVW-DAT      PIC  X(010).
             10 PI-DRPA0094-KNR-MANDANT       PIC S9(004) BINARY.
220415       10 PI-DRPA0094-PDTA-UST-BETRAG   PIC S9(009)V9(2)
220415                                        PACKED-DECIMAL.
100717       10 PI-DRPA0094-PDTA-BUCH-DAT     PIC  X(010).
          05 PI-DRPA0094-PDTA-LAST-UPDATE     PIC  X(026).
          05 PI-DRPA0094-PDTA-KENNUNG         PIC  X(008).
      *
       01 PO-DRPA0094.
          05 PO-DRPA0094-RC                   PIC  X(003).
             88 PO-DRPA0094-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0094-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ***** Ende-Copy-Member *****************************************


