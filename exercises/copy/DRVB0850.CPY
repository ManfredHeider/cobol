      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.03.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0850.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0850.CPv  $
      *
      *   Rev 2.4   10 Oct 2007 17:15:46   t13ex13
      *AVB00906: Einführung EU-Verm.RL im VB-System: Abrechnung
      *
      *   Rev 2.3   08 Oct 2007 17:10:28   t13ex13
      *AVB00906: Einführung EU-Verm.RL im VB-System: Abrechnung
      *
      *   Rev 2.2   22 Aug 2007 17:21:20   t13ex13
      *AVB00906: Einführung EU-Verm.RL im VB-System
      *
      *   Rev 2.1   10 Jul 2007 11:13:36   t13ex13
      *AVB00906: Einführung EU-Verm.RL im VB-System: Feedbackverarb
      *eitung
      *
      *   Rev 2.0   27 Mar 2007 11:15:04   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
       01  DRVB0850-SATZ.
           05 DRVB0850-VVR-MELDE-AUS-SST.
              07 DRVB0850-SATZART                PIC XXX.
              07 DRVB0850-MELDESTATUS            PIC XXX.
              07 DRVB0850-FILLER-FILLER.
                 09 FILLER              OCCURS 640 TIMES
                                        PIC X.
              07 DRVB0850-MELDE-INFO-FILLER
                                    REDEFINES DRVB0850-FILLER-FILLER.
                 09 DRVB0850-VVR-MELDE-AUS-INFO.
                    11 DRVB0850-ROUTETOVU        PIC S9(8)      BINARY.
                    11 DRVB0850-MELDUNGVU        PIC S9(8)      BINARY.
                    11 DRVB0850-REGNR            PIC X(30).
                    11 DRVB0850-VUVERMITTLERKEY  PIC X(20).
                    11 DRVB0850-MELDUNGKNR       PIC 999.
                    11 DRVB0850-INFOCODE         PIC S9(8)      BINARY.
                    11 DRVB0850-INFOTEXT         PIC X(255).
                    11 DRVB0850-MELDERSTATUS     PIC X(10).
                    11 DRVB0850-CREATETIME       PIC X(35).
                    11 DRVB0850-EXTERNALMESSAGEID PIC X(30).
                 09 FILLER                       PIC X(245).
              07 DRVB0850-MELDE-ERROR-FILLER
                                    REDEFINES DRVB0850-FILLER-FILLER.
                 09 DRVB0850-VVR-MELDE-AUS-ERROR.
                    11 DRVB0850-ROUTETOVU        PIC S9(8)      BINARY.
                    11 DRVB0850-MELDUNGVU        PIC S9(8)      BINARY.
                    11 DRVB0850-REGNR            PIC X(30).
                    11 DRVB0850-VUVERMITTLERKEY  PIC X(20).
                    11 DRVB0850-ERRORCODE        PIC S9(8)      BINARY.
                    11 DRVB0850-ERRORTEXT        PIC X(255).
                    11 DRVB0850-ERRORMESSAGE     PIC X(255).
                    11 DRVB0850-CREATETIME       PIC X(35).
                    11 DRVB0850-EXTERNALMESSAGEID PIC X(30).
                 09 FILLER                       PIC X(03).
              07 DRVB0850-MELDE-LOE-FILLER
                                    REDEFINES DRVB0850-FILLER-FILLER.
                 09 DRVB0850-VVR-MELDE-AUS-LOE.
                    11 DRVB0850-ROUTETOVU        PIC S9(8)      BINARY.
                    11 DRVB0850-MELDUNGVU        PIC S9(8)      BINARY.
                    11 DRVB0850-REGNR            PIC X(30).
                    11 DRVB0850-VUVERMITTLERKEY  PIC X(20).
                    11 DRVB0850-CREATETIME       PIC X(35).
                    11 DRVB0850-EXTERNALMESSAGEID PIC X(30).
                 09 FILLER                       PIC X(517).
              07 DRVB0850-MELDE-LOE-LIST-FILLER
                                    REDEFINES DRVB0850-FILLER-FILLER.
                 09 DRVB0850-MELDE-LOE-LIST.
                    11 DRVB0850-ROUTETOVU        PIC S9(8)      BINARY.
                    11 DRVB0850-MELDUNGVU        PIC S9(8)      BINARY.
                    11 DRVB0850-MESSAGEID        PIC X(50).
                    11 DRVB0850-PARTIALMESSAGENO PIC S9(08)     BINARY.
                    11 DRVB0850-PARTIALMESSAGECOUNT
                                                 PIC S9(08)     BINARY.
                    11 DRVB0850-DATUMERSTELLUNG  PIC X(10).
                    11 DRVB0850-REGNR            PIC X(30).
                    11 DRVB0850-VUVERMITTLERKEY  PIC X(20).
                    11 DRVB0850-NAME             PIC X(255).
                    11 DRVB0850-DATUMLOESCHUNG   PIC X(10).
                    11 DRVB0850-LOESCHUNGVON     PIC X(10).
                    11 DRVB0850-LOESCHUNGBIS     PIC X(10).
                    11 DRVB0850-CREATETIME       PIC X(35).
                 09 FILLER                       PIC X(194).
              07 VVR-MELDE-AUS-GEBUEHR-FILLER
                                    REDEFINES DRVB0850-FILLER-FILLER.
                 09 VVR-MELDE-AUS-GEBUEHR.
                    11 DRVB0850-ROUTETOVU        PIC S9(8)      BINARY.
                    11 DRVB0850-MELDUNGVU        PIC S9(8)      BINARY.
                    11 DRVB0850-MESSAGEID        PIC X(50).
                    11 DRVB0850-PARTIALMESSAGENO PIC S9(08)     BINARY.
                    11 DRVB0850-PARTIALMESSAGECOUNT
                                                 PIC S9(08)     BINARY.
                    11 DRVB0850-RECHNUNGSBETRAG  PIC 9(9)V99.
                    11 DRVB0850-REGNR            PIC X(30).
                    11 DRVB0850-VUVERMITTLERKEY  PIC X(20).
                    11 DRVB0850-BETRAG           PIC 9(9)V99.
                    11 DRVB0850-TEXT-RECHNUNG    PIC X(255).
                    11 DRVB0850-DATUM            PIC X(10).
                    11 DRVB0850-RGNR-EINZEL      PIC X(30).
                    11 DRVB0850-RECHNUNGSNUMMER  PIC X(30).
                    11 DRVB0850-RECHNUNGSDATUM   PIC X(10).
                    11 DRVB0850-CREATETIME       PIC X(35).
                 09 FILLER                       PIC X(132).
      *
      **************************************************************
