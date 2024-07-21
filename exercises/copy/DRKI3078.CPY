      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.07.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3078.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3078.CPv  $
      *
      *   Rev 2.1   Aug 22 2016 13:06:20   t13ex73
      *AKI01273: Abgleich von Kundenunterschieden zwischen OKI und 
      *HOST - Arbeitspaket 2
      *
      *   Rev 2.0   Jul 28 2016 10:04:08   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 3 0 7 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER PMKI1303 EINGABEDATEI                        *
      *                                                                *
      *    SATZLAENGE  200 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      AENDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *28.07.16!NEUERSTELLUNG                          !DDY      ! 01  *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *    Kundehostid
           05 'XX'-KUNDEHOSTID                  PIC  X(026).
      *    VB Nummer
           05 'XX'-VBNUMMER                     PIC  9(007).
      *    Verarbeitungs KZ
           05 'XX'-VERARB-KZ                    PIC  X(001).
           05 FILLER                            PIC  X(166).

