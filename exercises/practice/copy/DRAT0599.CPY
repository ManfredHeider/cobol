      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0599.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0599.CPv  $
      *
      *   Rev 2.3   Apr 12 2018 09:27:38   t13ae23
      *AAN01354: DVAG-Wien: Erfassung mehrere "Inhaber" in der Antr
      *agserf.
      *
      *   Rev 2.2   Mar 26 2018 11:17:14   t13ae23
      *AAN01354: DVAG-Wien: Erfassung mehrer "Inhaber" in der Antra
      *gserf.
      *
      *   Rev 2.1   Mar 20 2018 10:40:54   t13ae23
      *AAN01354: DVAG-Wien: Erfassung mehrer "Inhaber" in der Antra
      *gserf.
      *
      *   Rev 2.0   Sep 04 2015 15:26:48   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * COPY-MEMBER: DRAT0599  KONSTANTEN FÜR OE-KUNDE BERUFSGRUPPE  *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    03.01.2018                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01354!03.01.18!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-OE-KUNDE-BGRP-SELBSTSTAEND PIC S9(004) BINARY SYNC
                                                       VALUE  1.
           05 K-OE-KUNDE-BGRP-ANGESTELLT   PIC S9(004) BINARY SYNC
                                                       VALUE  2.
           05 K-OE-KUNDE-BGRP-ARBEITER     PIC S9(004) BINARY SYNC
                                                       VALUE  3.
           05 K-OE-KUNDE-BGRP-BEAMTE       PIC S9(004) BINARY SYNC
                                                       VALUE  4.
           05 K-OE-KUNDE-BGRP-HAUS-M-F     PIC S9(004) BINARY SYNC
                                                       VALUE  5.
           05 K-OE-KUNDE-BGRP-KIND         PIC S9(004) BINARY SYNC
                                                       VALUE  6.
           05 K-OE-KUNDE-BGRP-LANDWIRT     PIC S9(004) BINARY SYNC
                                                       VALUE  7.
           05 K-OE-KUNDE-BGRP-RENTNER      PIC S9(004) BINARY SYNC
                                                       VALUE  8.
           05 K-OE-KUNDE-BGRP-STUDENT      PIC S9(004) BINARY SYNC
                                                       VALUE  9.



