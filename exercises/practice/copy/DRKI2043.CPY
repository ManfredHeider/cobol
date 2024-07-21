      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2043.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2043.CPv  $
      *
      *   Rev 2.6   19 Nov 2007 09:54:34   t13ex27
      *AKL00664: Allfinanz: Anpassung Kundenaktualisierung
      *
      *   Rev 2.5   18 Nov 2007 15:22:24   t13ex27
      *No change.
      *
      *   Rev 2.4   16 Oct 2007 14:24:42   t13ex27
      *AKL00664: Allfinanz: Anpassung Kundensuche
      *
      *   Rev 2.3   Aug 24 2007 08:38:52   t13ex14
      *AKL00664: Allfinanz: KFZ DTA-Kontrolle Mandantenwechsel
      *
      *   Rev 2.2   Aug 15 2007 11:16:14   t13ex14
      *AKI00664: Allfinanz: KFZ DTA-Kontrolle Mandantenwechsel
      *
      *   Rev 2.1   Aug 02 2007 16:10:46   t13ex53
      *AKI00664: Allfinanz: KFZ DTA-Kontrolle Mandantenwechsel
      *
      *   Rev 2.0   Jul 04 2007 15:03:06   t13ex53
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRKI2043   Funktion Mandant, Lese Vertrag    *
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
240807* AKI00664!24.07.07!2.0 ! ABSCHLUSSVBNR             !W.Misgeld *
      *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI2043.
           05  PI-DRKI2043-VNR                PIC X(15).
           05  PI-DRKI2043-PGEB-GES           PIC S9(4) BINARY.

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI2043.
           05  PO-DRKI2043-RC                  PIC  X(03).
               88  PO-DRKI2043-RC-OK           VALUE 'OK '.
               88  PO-DRKI2043-RC-NFD          VALUE 'NFD'.
           05 PO-DRKI2043-VERTRAGS-ID          PIC  X(13).
           05 PO-DRKI2043-MANDANTNR            PIC  S9(9) BINARY.
240807     05 PO-DRKI2043-ABSCHLUSSVBNR        PIC  S9(9) BINARY.

