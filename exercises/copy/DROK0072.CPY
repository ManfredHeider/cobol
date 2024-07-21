 
      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0072.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DROK0072.CPv  $
      *
      *   Rev 2.10   Mar 31 2016 08:21:40   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.9   Feb 12 2016 08:26:46   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.7   Nov 19 2015 08:17:26   t13ae34
      *AAN01211: Datentyp EA_PIN in Elektr_Antraege ändern
      *
      *   Rev 2.6   Nov 09 2015 15:23:22   t13ae34
      *AAN01211: Datentyp EA_PIN in Elektr_Antraege ändern
      *
      *   Rev 2.5   May 28 2015 09:54:48   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.4   Apr 23 2015 13:25:10   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.3   29 Jul 2008 16:21:08   T13EX32
      *AKI00733: Elektr. Anträge. Anpassen an neue Datenlänge für A
      *dvocard
      *
      *   Rev 2.2   07 Apr 2008 14:04:08   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.1   12 Jul 2007 16:06:16   t13ex43
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.0   22 Feb 2007 14:10:44   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0072                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LIST-OZP ZXOK0016                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    22.02.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AKI00556!22.02.07!2.1 ! NEU                       ! KLEEMEIER *
      * AAN01211!27.02.15!2.4 ! ELEKTRA                   ! HEIDER    *
      * AAN01211!09.11.15!2.6 ! EA_PIN DECIMAL 10         ! HEIDER    *
      * AAN01230!12.02.16!2.9 ! RÜCKBAU ELEKTR_ANTRAEGE   ! HEIDER    *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DROK0072 - ERMITTELN NOCH ZU VERARBEIT. ANTRÄGE   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DROK0072.
           05  PI-DROK0072-UFKT-CODE              PIC  9(002).
           05  PI-DROK0072-EA-VA-KZ               PIC  X(001).
           05  PI-DROK0072-GESELL                 PIC  9(004).
      *
       01  PO-DROK0072.
           05  PO-DROK0072-RC                     PIC  X(003).
               88  PO-DROK0072-RC-OK                  VALUE 'OK '.
               88  PO-DROK0072-RC-EOF                 VALUE 'EOF'.
               88  PO-DROK0072-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DROK0072-NI-TCOK0016.
               10  PO-DROK0072-NI-EA-DATEINAME    PIC S9(04) BINARY.
               10  PO-DROK0072-NI-EA-STATUS-KTY   PIC S9(04) BINARY.
               10  PO-DROK0072-NI-EA-STATUS-KNR   PIC S9(04) BINARY.
               10  PO-DROK0072-NI-EA-GES-KTY      PIC S9(04) BINARY.
               10  PO-DROK0072-NI-EA-GES-KNR      PIC S9(04) BINARY.
               10  PO-DROK0072-NI-EA-KUNDE-NR     PIC S9(04) BINARY.
               10  PO-DROK0072-NI-EA-MITARB-NR    PIC S9(04) BINARY.
               10  PO-DROK0072-NI-EA-AUSLESE-JOB  PIC S9(04) BINARY.
               10  PO-DROK0072-NI-LETZTE-AENDERUN PIC S9(04) BINARY.
               10  PO-DROK0072-NI-EA-VA-KZ        PIC S9(04) BINARY.
               10  PO-DROK0072-NI-EA-VERARBEITET  PIC S9(04) BINARY.
               10  PO-DROK0072-NI-EA-BLACK-BOX-DT PIC S9(04) BINARY.
270215         10  PO-DROK0072-NI-EA-ESIGN        PIC S9(04) BINARY.
270215         10  PO-DROK0072-NI-EA-PIN          PIC S9(04) BINARY.
          05  PO-DROK0072-DCL-TCOK0016.
              10  PO-DROK0072-EA-DATEINAME        PIC  X(50).
              10  PO-DROK0072-EA-STATUS-KTY       PIC S9(04) BINARY.
              10  PO-DROK0072-EA-STATUS-KNR       PIC S9(04) BINARY.
              10  PO-DROK0072-EA-GES-KTY          PIC S9(04) BINARY.
              10  PO-DROK0072-EA-GES-KNR          PIC S9(04) BINARY.
              10  PO-DROK0072-EA-KUNDE-NR         PIC S9(09) BINARY.
              10  PO-DROK0072-EA-MITARBEITER-NR   PIC S9(09) BINARY.
              10  PO-DROK0072-EA-AUSLESE-JOB      PIC S9(18)V COMP-3.
              10  PO-DROK0072-LETZTE-AENDERUNG    PIC  X(26).
              10  PO-DROK0072-EA-VA-KZ            PIC  X(01).
              10  PO-DROK0072-EA-VERARBEITET-AM   PIC  X(26).
              10  PO-DROK0072-EA-BLACK-BOX-DATA.
                 15 PO-DROK0072-EA-BLACK-BOX-LEN  PIC S9(04) BINARY.
                 15 PO-DROK0072-EA-BLACK-BOX-TXT  PIC  X(32000).
270215        10  PO-DROK0072-EA-ESIGN            PIC X(1).
270215        10  PO-DROK0072-EA-PIN              PIC S9(10)V COMP-3.
      ****
      **** ENDE COPY DROK0072 ****************************************
      ****
 


