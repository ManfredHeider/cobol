      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.02.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2849.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2849.CPv  $
      *
      *   Rev 2.1   Mar 24 2014 14:04:24   t13ex73
      *AKI01273: Abgleich von Kundenunterschieden zwischen OKI und 
      *HOST
      *
      *   Rev 2.0   Feb 25 2014 10:53:20   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 8 4 9    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER UNLOAD DER HOSTTABELLE KUNDENINTVBBEZ        *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   67 Bytes                                       *
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
               10  'XX'-FILLER-1           PIC  X(005).
               10  'XX'-PERSONENID         PIC  X(026).
               10  'XX'-LOESCH-KZ          PIC  X(001).
               10  'XX'-NI-LOESCH-KZ       PIC  X(001).
                   88 'XX'-LOESCH-KZ-IST-NULL          VALUE '?'.
               10  'XX'-ANZ-BETRUER        PIC  9(004) COMP.
               10  'XX'-KI-LETZTE-AEND-TS  PIC  X(026).
               10  'XX'-FILLER-2           PIC  X(001).
               10  'XX'-PMB-LETZTE-AEND-TS PIC  X(026).
               10  'XX'-FILLER-3           PIC  X(001).
      *
