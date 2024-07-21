      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0128.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRPA0128.CPv  $
      *
      *   Rev 2.3   May 18 1999 10:17:20   T13AE17
      *FAN00212 - 2. Übergabe der neuen Sourcen nach Rücknahme wg. ABEND
      *
      *   Rev 2.1   Apr 20 1999 14:37:12   T13AE17
      *AAN00219 - Einheiten nun mit 2 NK-Stellen
      *
      *   Rev 2.0   Apr 20 1999 10:45:26   T13AE17
      *nun Ansi-Source
      *
      *   Rev 2.0   Apr 19 1999 12:52:28   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:01:46   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0128  FUNKTION EINHEITEN BERECHNEN       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                         *
      *    DATUM:    11.09.1996                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !11.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !        !    !                           !          *
      * APA00012!20.04.99! 2.1! EINHEITEN AUF 2-NACHKOMMA-! PARENT   *
      *         !        !    ! STELLEN UMGESTELLT        !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0128                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0128.
          05 PI-DRPA0128-PZVP-VERSION     PIC S9(004)    BINARY.
          05 PI-DRPA0128-DATEN.
             10 PI-DRPA0128-GRUND-PROV    PIC S9(009)V99 PACKED-DECIMAL.
             10 PI-DRPA0128-GES-PROV      PIC S9(009)V99 PACKED-DECIMAL.
             10 PI-DRPA0128-KONDITION.
                15 PI-DRPA0128-PKON-KOND-NR   PIC S9(004)   BINARY.
                15 PI-DRPA0128-PKON-KOND-ART  PIC S9(004)   BINARY.
                15 PI-DRPA0128-KNR-REGEL      PIC S9(004)   BINARY.
                15 PI-DRPA0128-PKON-SATZ-1    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0128-PKON-SATZ-2    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0128-PKON-SATZ-3    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0128-PKON-SATZ-4    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0128-PKON-SATZ-5    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0128-PKON-SATZ-6    PIC S9(003)V99
                                                     PACKED-DECIMAL.
                15 PI-DRPA0128-PKON-WERT-1    PIC S9(009)   BINARY.
                15 PI-DRPA0128-PKON-WERT-2    PIC S9(009)   BINARY.
                15 PI-DRPA0128-PKON-WERT-3    PIC S9(009)   BINARY.
                15 PI-DRPA0128-PKON-WERT-4    PIC S9(009)   BINARY.
                15 PI-DRPA0128-PKON-WERT-5    PIC S9(009)   BINARY.
                15 PI-DRPA0128-PKON-WERT-6    PIC S9(009)   BINARY.
      *
       01 PO-DRPA0128.
          05 PO-DRPA0128-RC               PIC  X(003).
             88 PO-DRPA0128-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0128-RC-REGEL-FALSCH              VALUE 'REF'.
             88 PO-DRPA0128-RC-K-WERT-FEHLT              VALUE 'KWF'.
          05 PO-DRPA0128-DATEN.
200499       10 PO-DRPA0128-EINH          PIC S9(006)V99 PACKED-DECIMAL.
200499       10 PO-DRPA0128-EINH-VWF-VTF  PIC S9(006)V99 PACKED-DECIMAL.
200499       10 PO-DRPA0128-EINH-AVERM    PIC S9(006)V99 PACKED-DECIMAL.
200499       10 PO-DRPA0128-EINH-VM       PIC S9(006)V99 PACKED-DECIMAL.
200499       10 PO-DRPA0128-EINH-KDBETR   PIC S9(006)V99 PACKED-DECIMAL.
200499       10 PO-DRPA0128-EINH-STR-REST PIC S9(006)V99 PACKED-DECIMAL.
      *
      ******** ENDE COPY-MEMBER **************************************

