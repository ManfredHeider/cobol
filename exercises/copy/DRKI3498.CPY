      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3498.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3498.CPv  $
      *
      *   Rev 2.3   Nov 29 2018 08:54:46   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI 
      *
      *   Rev 2.2   Nov 28 2018 16:49:54   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI
      *
      *   Rev 2.1   Nov 27 2018 11:58:30   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI
      *
      *   Rev 2.0   Sep 18 2018 14:36:50   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3498                                      *
      *    --------------------------------------------------------   *
      *    Unload zu archivierender Verträge                          *
      *                                                               *
      *                                                               *
      *                                                               *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01600!18.09.18!2.1 ! neu                       ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
            05  'XX'-VERVERTRAGSID           PIC  X(013).
            05  'XX'-VERPARTGESELLSCHID      PIC S9(4) USAGE COMP.
            05  'XX'-VERVERTRAGSNUMMER       PIC  X(015).
            05  'XX'-NI-VERVERTRAGSNUMMER    PIC  X(001).
            05  'XX'-VERVERTRZUSTANDID       PIC S9(4) USAGE COMP.
            05  'XX'-NI-VERVERTRZUSTANDID    PIC  X(001).
            05  'XX'-VERDATUM                PIC  X(010).
      *              VERSTORNODATUM     oder
      *              VERVERTRZUSTANDDAT oder
      *              VERVERTRAGSABLAUF
            05  'XX'-NI-VERDATUM             PIC  X(001).
            05  'XX'-ORDERBEGR.
                49 'XX'-ORDERBEGR-LEN        PIC S9(4) COMP.
                49 'XX'-ORDERBEGR-TEXT       PIC  X(002).





