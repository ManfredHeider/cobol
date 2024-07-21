      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.06.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUMA0003.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUMA0003.CPv  $
      *
      *   Rev 2.2   Feb 09 2021 16:02:46   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.1   Oct 15 2013 11:08:42   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Jun 27 2013 14:12:20   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!27.06.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
       01  EXEC-DB2BT-IND-MAX    pic S9(4) binary sync value +6.
       01  EXEC-DB2BT.
           05  EXEC-VAR-01 pic X(80) value
CCC005*                        '//#VAR#### EXEC PROC=DB2BT#U,PROG=IKJEFT
      -                        '1B
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//GO.SYSTSIN  DD *
      -                        '
      -                        ''.
CCC004*    05  EXEC-VAR-02 pic X(80) value
CCC004*                        '    DSN SYSTEM(#VAR)
      -                        '
      -                        ''.
CCC004*    05  EXEC-VAR-03 pic X(80) value
CCC004*                        '    RUN  PROGRAM(#VAR####) +
      -                        '
      -                        ''.
CCC004*    05  EXEC-VAR-04 pic X(80) value
CCC004*                        '            PLAN(P2#U0000)
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '    END
      -                        '
      -                        ''.
      *
      **************************************************************

