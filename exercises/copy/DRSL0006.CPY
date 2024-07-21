      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0006.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRSL0006.CPv  $
      *
      *   Rev 2.1   06 Oct 2000 10:35:34   T13AE30
      *ASL00067 - FÖBO2
      *
      *   Rev 2.0   Feb 11 2000 15:19:48   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRSL0006
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS PGM PMSL0024                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH. WILLIG                                       *
      *    DATUM:    20.09.2000                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00067!20.09.00! 01 ! NEUERSTELLUNG             ! TH.WILLIG*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01   SST-FOEBO2.
            05  FB2-BETREUER              PIC  S9(009)   COMP.
            05  FB2-UNTST                 PIC  S9(009)   COMP.
            05  FB2-BEGINN                PIC   X(010).
            05  FB2-TYP-GRENZE            PIC  S9(009)V9 COMP-3.
            05  FB2-WERTUNGSZEITRAUM      PIC   X(001).
            05  FB2-ERSTELL-DAT           PIC   X(010).
            05  FILLER                    PIC   X(045).

      ***************** ENDE COPY-MEMBER DRSL0006 ********************
