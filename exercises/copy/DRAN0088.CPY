      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0088.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0088.CPv  $
      *
      *   Rev 2.2   Jan 07 2004 14:11:56   t13ae34
      *AAN00305 - Integration Sonderfälle eing. Geschäft (DRAN0088) 
      *in Daisy-Konfiguration
      *
      *   Rev 2.1   Jun 30 2003 10:16:10   t13ae34
      *AAN00369-mit Produkten, die auf Konfig. EG zugreifen
      *
      *   Rev 2.0   Mar 18 2002 09:16:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0088  KONSTANTEN                         *
      *    ENTHÄLT ALLE PRODUKTE, DEREN GRUNDPROVISION AUCH BEI      *
      *    NUTZUNG DER DAISY-KONFIGURATION AUS DER KONFIGURATION     *
      *    DES EINGEREICHTEN GESCHÄFTS (UU927S.CPY) GEZOGEN WERDEN   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00268!23.11.00!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00305!06.01.04!2.2 ! UMSTELLUNG AUF DAISY-KONF ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *FORMAT GGPP -> GG=GESELLSCHAFT, PP=PRODUKT                    *
      ****************************************************************
       01  Z-PRODUKTE-KONFIGURATION  PIC  X(004).
      *>>>>>
           88  PRODUKTE-EG-KONF      VALUE  'XXXX'.
      *    88  PRODUKTE-EG-KONF      VALUE  '4303'
      *                                    ,'4855'
      *                                    ,'4856'
      *                                    ,'4857'
      *                                    ,'6006'
      *                                    ,'6007'
      *                                    ,'7502'
      *                                    ,'8103'
      *                                    ,'8504'.
      *<<<<<<
