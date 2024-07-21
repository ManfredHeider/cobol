      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.02.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2285.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2285.CPv  $
      *
      *   Rev 2.5   Mar 27 2017 11:58:24   t13ex39
      *AKI01494: FoDB Premium Depot 
      *
      *   Rev 2.4   Mar 26 2017 08:51:08   t13ex39
      *AKI01494: FoDB Premium Depot
      *
      *   Rev 2.3   Mar 26 2017 08:40:38   t13ex39
      *AKI01494: FoDB Premium Depot
      *
      *   Rev 2.2   Mar 26 2017 08:27:20   t13ex39
      *AKI01494: FoDB Premium Depot
      *
      *   Rev 2.1   Jun 15 2009 16:33:56   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik
      *
      *   Rev 2.0   Feb 18 2009 13:14:54   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00740!15.02.09! 01 ! NEUERSTELLUNG             ! HORNUNG  *
      * -------------------------------------------------------------*
010217* AKI01494!01.02.17!    ! Premiumdepot bzw.         ! WHO      *
      *         !        !    ! Premiummandat             !          *
      *         !        !    ! REVERS-Felder entfernt    !          *
      *         !        !    ! GHP-VERFUEGUNGS-KZ entfern!          *
      * DBO-9247!01.10.21!    ! RISKOKLASSE WIEDER AUFGE- ! T13EX39  *  
      *         !        !    ! NOMMEN                    !          *  
      *  
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *                                                              *
      *                                                              *
      ****************************************************************
       01  DRKI2285.

           05  GHP-GES                     PIC  9(02).
           05  GHP-VNR                     PIC  9(15).

010217     05  GHP-DEPOT-ART               PIC  X(02).
           05  GHP-RISIKO-KLASSE           PIC 9(001).
010217     05  FILLER                      PIC X(045).

      ******************************************************************




