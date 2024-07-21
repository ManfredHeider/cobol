      *---------------------------------------------------------------*
      *    Standard f„r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:ÖPROFILEÖKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0131.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drki0131.cpv  $
      *
      *   Rev 2.2   Oct 28 1999 14:30:14   T13EX19
      *aki00054 - VerwaltungsPC Typ eingefügt
      *
      *   Rev 2.1   Mar 23 1999 15:57:38   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.2   14 May 1998 12:23:08   P13SP99
      *Neuerstellung
      *
      *   Rev 1.1   14 May 1998 10:34:00   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:50:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0131                                      *
      *    --------------------------------------------------------   *
      *    STANDARD-KONSTANTEN SPEZIELL F—R DAS PROJEKT 'KI'          *
      *                                                               *
      *    IM DRAT0009 WERDEN ALLGEMEINE KONSTANTEN DEFINIERT, DIE    *
      *    UNABHŠNGIG VOM PROJEKT G„LTIG SIND. HIER DAGEGEN WERDEN    *
      *    NUR SOLCHE KONSTANTEN DEFINIERT, DIE INNERHALB VON 'KI'    *
      *    G„LTIG SIND.                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: E. SCH˜FER                                       *
      *    DATUM:    29.04.1998                                       *
      *****************************************************************
      *    VERSION   1    29.04.98 16:19:49
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG Ü DATUM  ÜVER Ü AENDERUNG                 Ü NAME     *
      * -------------------------------------------------------------*
      * AKI00001Ü29.04.98Ü 01 Ü NEU                       Ü SCH˜FER  *
      * AKI00001Ü11.05.98Ü 02 Ü EINBAU KONSTANTE 2 UND M  Ü HUEBNER  *
      * AKI00001Ü25.06.98Ü 03 Ü EINBAU GESCHLECHT KONST   Ü ZGUR-VUJIC
      * AKI00001Ü07.07.98Ü 04 Ü EINBAU AL-IN-PROBE-KZ     Ü ZGUR-VUJIC
      * AKI00001Ü07.07.98Ü 05 Ü EINBAU PC-TYP             Ü ZGUR-VUJIC
      * AKI00001Ü09.07.98Ü 06 Ü EINBAU ANSCHRIFT KONST    Ü ZGUR-VUJIC
      * AKI00001Ü03.08.98Ü 07 Ü EINBAU VERTRAGSBEZIEHUNGS-Ü SASCHA JOST
      *         Ü        Ü    Ü        ARTKENNZEICHEN     Ü          *
      *AKI00054 Ü21.10.99Ü    Ü Neues PC-TYp eingetragen  Ü Zgur-Vujic
      *         Ü        Ü    Ü                           Ü          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      ***  NUMERISCHE KONSTANTEN
       01  FILLER.

150798* ESC: KONSTANTE K-WERT-2 NICHT MEHR BENUTZEN, DA NICHT SPRECHEND.
150798* K-WERT-2 DARF NUR IN EINMALIGER AKTUALISIERUNG BENUTZT WERDEN.
110598***  STANDARD-WERT 2
           05  K-WERT-2                 PIC  9(001) VALUE 2.

030898***  VERTRAGSBEZIEHUNGSART-KENNZEICHEN NUMERISCH
030898     05  K-ZWEITE-VERSICHERTE-PERSON PIC  9(001) VALUE 2.

      ***  ALPHANUMERISCHE KONSTANTEN
       01  FILLER.

      ***  NULL-INDIKATOREN
      ***  VORSICHT: KONSTANTEN VON NULL-INDIKATOR-INHALTEN D—RFEN
      ***  ========  NICHT ZUR ABFRAGE AUF NULL-FELDER BENUTZT WERDEN,
      ***            SONDERN NUR ZUM SETZEN DES NULL-INDIKATORS.

      ***  NULL-WERT IN NULL-INDIKATOR IM LADEFORMAT (WUKInnnn) UND
      ***  NULL-WERT IN NULL-INDIKATOR IM DISPLAYFORMAT (WDKInnnn)
           05  K-LOAD-NULL              PIC  X(001) VALUE '?'.

      ***  NOT-NULL-WERT IN NULL-INDIKATOR IM LADEFORMAT (WUKINNNN)
           05  K-LOAD-NOT-NULL          PIC  S9(4) COMP VALUE 0.

      ***  NOT-NULL-WERT IN NULL-INDIKATOR IM DISPLAYFORMAT (WDKINNNN)
           05  K-PC-NOT-NULL            PIC  X(001) VALUE SPACE.

150798* ESC: KONSTANTE K-WERT-M NICHT MEHR BENUTZEN, DA NICHT SPRECHEND.
150798* K-WERT-M DARF NUR IN EINMALIGER AKTUALISIERUNG BENUTZT WERDEN.
110598***  STANDARD-WERT M
           05  K-WERT-M                 PIC  X(001) VALUE 'M'.

030898***  VERTRAGSBEZIEHUNGSART-KENNZEICHEN ALPHANUMERISCH
030898     05  K-MITVERSICHERTE-PERSON   PIC  X(001) VALUE 'M'.

250698***  GESCHLECHT
           05  K-MAENNLICH               PIC  X(001) VALUE 'M'.
           05  K-WEIBLICH                PIC  X(001) VALUE 'W'.
           05  K-GESCHL-UNBEKANNT        PIC  X(001) VALUE 'U'.

070798***  IN PROBE KENNZEICHEN F—R AL
           05  K-AL-IN-PROBE-KZ          PIC  X(001) VALUE 'P'.

070798***  PC-TYP für Arbeitsstation Tabelle
           05  K-PC-TYP-DESKTOP          PIC  X(001) VALUE 'D'.
           05  K-PC-TYP-THINKPAD         PIC  X(001) VALUE 'T'.
           05  K-PC-TYP-VERWALTUNG       PIC  X(001) VALUE 'V'.

090798***  ANSCHRIFT KONSTANTEN
           05  K-ANSCHR-BUERO            PIC  9(001) VALUE 4.
           05  K-ANSCHR-PRIVAT           PIC  9(001) VALUE 1.

      ******** ENDE COPY-MEMBER STANDARD-KONSTANTEN (DRKI0131) ********
