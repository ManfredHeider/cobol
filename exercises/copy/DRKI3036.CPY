      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.09.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3036.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3036.CPv  $
      *
      *   Rev 2.1   Sep 22 2015 11:06:26   t13ex73
      *AKI01273: Abgleich von Kundenunterschieden zwischen OKI und 
      *HOST - Vertrag PGM + JCL
      *
      *   Rev 2.0   Sep 16 2015 12:54:34   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 3 0 3 6    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER UNLOAD DER VERTRAG-BEZ Tabellen   HOST       *
      *                                                                *
      *                                                                *
      *    SATZLAENGE  79 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *05.08.15!NEUERSTELLUNG                          !DDY      ! 01  *
      *AKI01273!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *01  DRKI3033.
      *
           05  'XX'-DATENSATZ.
               10 'XX'-KEY.
                  15  'XX'-VERTRAGSID              PIC  X(026).
                  15  'XX'-PERSONENID              PIC  X(026).
               10  'XX'-LETZTE-AEND-TS          PIC  X(026).
               10  FILLER                       PIC  X(001).
      *

