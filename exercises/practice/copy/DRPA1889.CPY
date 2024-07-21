      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  29.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1889.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1889.CPv  $
      *
      *   Rev 2.1   May 22 2018 09:48:32   t13ae46
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.0   Mar 29 2018 13:33:00   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *COPY-MEMBER: DRPA1889  PEG-DATENSATZ GES. 21 - ALAG LEBEN      *
      *                                                               *
      *---------------------------------------------------------------*
      *    ERSTELLT: WANG                                             *
      *    DATUM:    29.03.2018                                       *
      *****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02579!        !2.1 ! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    ALAG-LEBEN DTAS.                                          *
      ****************************************************************
      *
             30 ALAG-LEBEN-DATENSATZ.
                35 PGEB-GES                 PIC  9(02).
                35 HERKUNFT                 PIC  X(02).
                35 VNR                      PIC  9(15).
                35 KENNUNG                  PIC  X(06).
                35 VERTRAG-BEGINN           PIC  X(10).
                35 VERTRAG-ABLAUF           PIC  X(10).
                35 POLICIER-DATUM           PIC  X(10).
                35 FAELLIG-VON              PIC  X(10).
                35 INKASSO-STAND            PIC  X(10).
                35 HAFTZEIT-VON             PIC  X(10).
                35 HAFTZEIT-BIS             PIC  X(10).
                35 AVERM                    PIC  9(07).
                35 VN-NNAME                 PIC  X(25).
                35 VN-VNAME                 PIC  X(20).
                35 VP-NNAME                 PIC  X(25).
                35 VP-VNAME                 PIC  X(20).
                35 ABR-DAT-PG               PIC  X(10).
                35 WAEHRUNG                 PIC  X(03).
                35 ZAHLWEISE                PIC  X(02).
                35 JAHRESBEITRAG            PIC S9(07)V9(02).
                35 BZ-DAUER                 PIC  9(04).
                35 VERTRAGSDAUER            PIC  9(04).
                35 DYNAMIK                  PIC  9(01).
                35 GEVO                     PIC  X(07).
                35 PG-PGV                   PIC  X(04).
                35 PG-PRODUKTBEZEICHNUNG    PIC  X(20).
                35 PG-TARIF                 PIC  X(03).
                35 BEW-SUMME                PIC S9(07)V9(02).
                35 PRO-RATA-SUMME           PIC S9(07)V9(02).
                35 PROV-SATZ                PIC S9(03)V9(03).
                35 PROV-BETRAG              PIC S9(07)V9(02).
                35 PRO-RATA-FAKTOR          PIC  9(06).
                35 RABATTSTUFE              PIC  X(05).
                35 EINALTER1                PIC  9(04).
      *
      ******* ENDE COPY-MEMBER **************************************

