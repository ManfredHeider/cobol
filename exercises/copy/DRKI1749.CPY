      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.01.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1749.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1749.CPv  $
      *
      *   Rev 2.2   May 09 2006 17:34:32   t13ex27
      *AKI00518: EDI-Metadatensätze von der AMV
      *
      *   Rev 2.1   Apr 01 2005 12:10:44   t13ex48
      *AKI00460: e-Post: Bereitstellg. der tägl. Änd. im Betreuungs
      *verhältnis
      *
      *   Rev 2.0   Jan 12 2005 15:01:48   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1749                                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HSE                                             *
      *    DATUM:    21.02.2005                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00460!21.02.05! 01 ! NEUERSTELLUNG             ! HSE      *
      * -------------------------------------------------------------*
011205* AKI00518!01.12.05! 01 ! Anpassung VNR auf 12 Stel.! HSE      *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE Satzbeschreibung für die Aend.-   *
      *    SST aus Kunden-Übertragungen zur EDI-Meta-DB              *
      *                                                              *
      ****************************************************************
      *
       01 DRKI1749.
          05 EDI-Aend-DATEN.
             10 EDI-Ges                          PIC  9(002).
011205*      10 EDI-VNR                          PIC  X(010).
011205       10 EDI-VNR                          PIC  X(012).
             10 EDI-VB-alt                       PIC  9(007).
             10 EDI-VB-neu                       PIC  9(007).
             10 EDI-Betr-Status                  PIC  X(001).
      *
      ****** Ende Copy-Member ****************************************
