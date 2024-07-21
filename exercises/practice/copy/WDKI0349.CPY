      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   wdki0349.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\wdki0349.cpv  $
      *
      *   Rev 2.2   10 May 2000 08:59:16   T13AE38
      *AKI00106
      *
      *   Rev 2.1   Apr 16 1999 10:10:42   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:42   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00106 !13.04.00!    !-VKBKLAUSELID              !S. JOST   *
      *         !        !    ! JETZT INTEGER             !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        05 EA-VERSKLAUSELBEZ-GESAMT.
         10 EA-NI-VERSKLAUSELBEZ-TAB.
           20 EA-NI-VERSKLAUSELBEZ-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-VERSKLAUSELBEZ
                 REDEFINES EA-NI-VERSKLAUSELBEZ-TAB.
           20 EA-NI-VKBVERTRAGSID            PIC X.
           20 EA-NI-VKBVERSTARLFDNR          PIC X.
           20 EA-NI-VKBKLAUSELID             PIC X.
           20 EA-NI-VKBKLAUSELLFDNR          PIC X.
           20 EA-NI-VKBKLAUSELPARTGID        PIC X.
           20 EA-NI-VKBKLAUSEL               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-VERSKLAUSELBEZ.
           20 EA-VKBVERTRAGSID               PIC X(13).
           20 EA-VKBVERSTARLFDNR             PIC -----9    .
130400*    20 EA-VKBKLAUSELID                PIC -----9    .
130400     20 EA-VKBKLAUSELID                PIC --------9 .
           20 EA-VKBKLAUSELLFDNR             PIC -----9    .
           20 EA-VKBKLAUSELPARTGID           PIC -----9    .
           20 EA-VKBKLAUSEL                  PIC X(10).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
