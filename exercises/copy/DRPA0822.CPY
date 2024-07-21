      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0822.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRPA0822.CPv  $
      *
      *   Rev 2.3   May 18 1999 10:17:36   T13AE17
      *FAN00212 - 2. Übergabe der neuen Sourcen nach Rücknahme wg. ABEND
      *
      *   Rev 2.1   Apr 20 1999 14:37:26   T13AE17
      *AAN00219 - Einheiten nun mit 2 NK-Stellen
      *
      *   Rev 2.0   Apr 20 1999 10:56:56   T13AE17
      *Umstellung MFE - nun Ansi-Source
      *
      *   Rev 2.0   Apr 19 1999 12:52:38   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.1   08 Aug 1998 11:32:30   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   14 Jan 1998 12:44:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0822 FUNKTION BERECHNEN GRUND-/GESAMT-   *
      *                    PROVISIONSSATZ UND EINHEITEN JE PRODUKT   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    11.02.1998                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00167!11.02.98! 01 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * APA00012!20.04.99! 2.1! EINHEITEN AUF 2-NACHKOMMA-! PARENT   *
      *         !        !    ! STELLEN UMGESTELLT        !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0822                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0822.
          05 PI-DRPA0822-DATEN.
             10 PI-DRPA0822-NETTO-BEITRAG        PIC S9(009)V99
                                                        PACKED-DECIMAL.
             10 PI-DRPA0822-PGEB-GES             PIC S9(004)  BINARY.
             10 PI-DRPA0822-PROD-PROD-SCHL       PIC S9(004)  BINARY.
             10 PI-DRPA0822-STICHTAG             PIC  X(010).
             10 PI-DRPA0822-PRODART              PIC S9(004)  BINARY.
             10 PI-DRPA0822-LAUFZEIT             PIC S9(004)  BINARY.
      *
       01 PO-DRPA0822.
          05 PO-DRPA0822-RC                      PIC  X(003).
             88 PO-DRPA0822-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0822-RC-NOK                    VALUE 'NOK'.
         05 PO-DRPA0822-DATEN.
             10 PO-DRPA0822-GRUND-PROV-KOND-NR   PIC S9(004) BINARY.
             10 PO-DRPA0822-GRUND-PROV           PIC S9(009)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0822-GSMT-PROV-KOND-NR    PIC S9(004) BINARY.
             10 PO-DRPA0822-GSMT-PROV            PIC S9(009)V99
                                                        PACKED-DECIMAL.
200499       10 PO-DRPA0822-EINH                 PIC S9(006)V99
                                                        PACKED-DECIMAL.
200499       10 PO-DRPA0822-EINH-VWF             PIC S9(006)V99
                                                        PACKED-DECIMAL.
200499       10 PO-DRPA0822-EINH-VTF             PIC S9(006)V99
                                                        PACKED-DECIMAL.
200499       10 PO-DRPA0822-EINH-AVERM           PIC S9(006)V99
                                                        PACKED-DECIMAL.
200499       10 PO-DRPA0822-EINH-VM              PIC S9(006)V99
                                                        PACKED-DECIMAL.
200499       10 PO-DRPA0822-EINH-KD-BETR         PIC S9(006)V99
                                                        PACKED-DECIMAL.
200499       10 PO-DRPA0822-EINH-REST-STRK       PIC S9(006)V99
                                                        PACKED-DECIMAL.
      *
      ******** ENDE COPY-MEMBER **************************************

