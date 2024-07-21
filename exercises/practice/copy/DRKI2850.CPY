      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.02.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2850.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2850.CPv  $
      *
      *   Rev 2.1   Mar 24 2014 14:04:32   t13ex73
      *AKI01273: Abgleich von Kundenunterschieden zwischen OKI und 
      *HOST
      *
      *   Rev 2.0   Feb 25 2014 10:53:26   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 8 5 0    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER UNLOAD DER OKI-TABELLE PERSON_MITARBEITER_BEZ*
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   61 Bytes                                        *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *25.02.14!NEUERSTELLUNG                          !DDY      ! 01  *
      *AKI01273!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *
           05  'XX'-DATENSTAZ.
               10  'XX'-MITARBEITERNR      PIC  9(007).
               10  'XX'-PERSONENID         PIC  X(026).
               10  'XX'-LOESCH-KZ          PIC  X(001).
               10  'XX'-ANZ-BETREUER       PIC  9(003).
               10  'XX'-KI-LETZTE-AEND     PIC  X(026).
               10  'XX'-PMB-LETZTE-AEND    PIC  X(026).
               10  'XX'-FILLER-1           PIC  X(001).
      *
