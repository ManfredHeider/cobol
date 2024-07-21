      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.01.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3117.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI3117.CPv  $
      *
      *   Rev 2.6   Mar 27 2017 11:58:08   t13ex39
      *AKI01494: FoDB Premium Depot 
      *
      *   Rev 2.5   Mar 26 2017 08:50:52   t13ex39
      *AKI01494: FoDB Premium Depot
      *
      *   Rev 2.4   Mar 26 2017 08:40:24   t13ex39
      *AKI01494: FoDB Premium Depot
      *
      *   Rev 2.3   Mar 26 2017 08:27:04   t13ex39
      *AKI01494: FoDB Premium Depot
      *
      *   Rev 2.2   Feb 02 2017 15:58:52   t13ex39
      *AKI01410: FodB Goldsparplan Upro überarbeiten
      *
      *   Rev 2.1   Jan 10 2017 10:53:50   t13ex39
      *AKI01410: FodB Goldsparplan Upro überarbeiten
      *
      *   Rev 2.0   Jan 04 2017 12:21:22   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3117                                      *
      *    --------------------------------------------------------   *
      *    COPY für Vertragstypen bzw. Kontoarten                     *
      *    --------------------------------------------------------   *
      ****************************************************************
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01410!22.12.16!    ! Copy neu enthält die Ver- ! WHO      *
      *         !        !    ! tragstypen bzw. Kontoarten!          *
      * -------------------------------------------------------------*
      * AK0I1494!01.02.17!    ! Copy VERTRAGSTPY 'PM'     ! WHO      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  S-VERTRAGSTYP             PIC  X(002)       VALUE SPACE.
                88 S-VERTRAGSTYP-GUELTIG-SPARPLAN   VALUES
                   'AV'
                   'PP'  'SV'
                   'VL'  'VP'
                   'FE'  'FA'
                   'DF'
                   'VA'  'VS'
                   'IE'  'PE'  'SE'
                   'IA'  'PA'  'SA' 'OV'.
               88 S-VERTRAGSTYP-ZAHLUNG-EIN            VALUE
                              'AV'   'VP'   'VL'
      *    CGO hat Depot mit DF in Prod nachgesehen, kein VL
                              'VS'  'VA' 'DF'

                              'FE'
                              'IE'   'PE'   'SE' 'OV'.
               88 S-VERTRAGSTYP-ZAHLUNG-AUS            VALUE
                              'IA'   'PA'   'SA'
                              'FA'              .
               88 S-VERTRAGSTYP-PLANPLUS-VL            VALUE
                                     'PP'   'SV' .
               88 S-VERTRAGSTYP-VL-VERTRAEGE           VALUE
                              'AV'   'VL'   'SV' 'OV'
                              'VS'   'VA'       .
               88 S-VERTRAGSTYP-GOLD-SPARPALN          VALUE
                              'GS'              .
100217         88 S-VERTRAGSTYP-PREMIUM-MANDAT         VALUE
100217                        'PM' 'PE' 'PA'    .









