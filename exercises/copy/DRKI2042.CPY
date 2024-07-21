      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2042.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2042.CPv  $
      *
      *   Rev 2.3   Oct 17 2007 09:42:14   t13ex14
      *AKL00664: Allfinanz: KFZ DTA-Kontrolle Mandantenwechsel
      *
      *   Rev 2.2   Aug 15 2007 11:16:04   t13ex14
      *AKI00664: Allfinanz: KFZ DTA-Kontrolle Mandantenwechsel
      *
      *   Rev 2.1   Jul 09 2007 11:41:58   t13ex53
      *AKI00664: Allfinanz: Bestandsdaten (inkl. AM-Vers FK) und Rü
      *ck-DTA Copy und CTKI
      *
      *   Rev 2.0   Jul 04 2007 15:04:10   t13ex53
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2042  Funktion Mandant, Schreibe Vertrag *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Parent                                          *
      *    DATUM:    04.07.2007                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00664!04.07.07!2.0 ! Neuerstellung             !W.Schmidt *
      *
150807* AKI00664!15.08.07!2.0 ! Mandant als INTEGER       !W.Misgeld *
      *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI2042.
           05 DRKI2042-VERTRAGS-ID            PIC  X(13).
           05 DRKI2042-LFDNR                  PIC  S9(4) BINARY.
150807*    05 DRKI2042-MANDANTNRNEU           PIC  S9(4) BINARY.
150807     05 DRKI2042-MANDANTNRNEU           PIC  S9(9) BINARY.
150807*    05 DRKI2042-MANDANTNRALT           PIC  S9(4) BINARY.
150807     05 DRKI2042-MANDANTNRALT           PIC  S9(9) BINARY.
           05 DRKI2042-VNR                    PIC  X(15).
           05 DRKI2042-PGEB-GES               PIC  S9(4) BINARY.

