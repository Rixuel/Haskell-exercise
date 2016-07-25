-- En utilisant uniquement les fonctions de base de Haskell (sans aucun 'import' de librairies), ecrivez les codes suivants:
-- Les fonctions doivent respecter le typage mentionn�
module TP1 where

-- Ex 1 (1pt)- Donnez une fonction "verif_n" qui prend deux cha�nes de caract�res et un entier n en param�tre, et d�termine si le n-i�me caract�re des deux cha�nes de caract�res est identique (sans utiliser le "if")
verif_n ::  String->String->Int->Bool
verif_n x y n = (drop (n)(take (n+1) x)) == [(y !! n)]


-- Ex 2(1pt)- Donnez une fonction recursive "position" qui retourne la valeur n si le n-i�me caracteres des deux chaines s1 et s2 est identique, sinon la fonction retourne la valeur -1
-- exemple
	-- position "paris" "sirop" 0 => 2
	-- position "waajdi" "idja" 0 => -1
position :: String->String->Int->Int
position x y n = 	if ((length x)-1<n || (length y)-1<n)==False
					then (	if [(x!!n)]==[(y!!n)]
							then n
							else position x y (n+1))
					else (-1)
					
					
-- Ex 3(1pts)- Ecrire la fonction recursive "inverse_chaine" qui donne l'inverse d'une chaine de caract�res (sans utiliser la fonction 'reverse')
-- exemple	inverse_chaine "wajdi" => "idjaw"
inverse_chaine :: String->String
inverse_chaine x 
	| (x==[])=[]
	| otherwise = inverse_chaine(tail x)++[head x]


-- Ex 4(2pts)- Ecrire la fonction recursive "partie_palyndrome" qui utilise la fonction "inverse_chaine" qui retourne la partie palydrome d'une chaine
-- exemple:	partie_palyndrome "wajdijaw" => "waj"
-- partie_palyndrome "wajdidjaw" => "wajdi"
-- partie_palyndrome "wajddjaw" => "wajd"

partie_palyndrome :: String->String 
partie_palyndrome x 
	| length x == 0 = [] 
	| (head x /= last x)=[] 
	| length x == 1 = x 
	| head x == head (inverse_chaine x ) = concat[[head x], partie_palyndrome(tail(init x))] 
	| otherwise = partie_palyndrome(tail(init x))	


-- Ex 5(2pts)- Ecrire la fonction recursive "impaire" qui prend une liste d'entier et retourne une liste contenant uniquement les impaires de lq premi�re liste
-- exemple impaire [1,3,4,2] => [1,3]
impaire :: [Int]->[Int]
impaire x = if x==[] then [] else  [ x | x <- x, x `mod` 2 == 1]

-- Ex 6(2pts)- Ecrire la fonction recursive "impaire_paire" qui prend une liste d'entier et retourne une liste contenant les elements impaires a gauche et les elements paires a droite (NB: le rang n'est pas important)
-- exemple impaire_paire [1,2,3,4,5] => [1,3,5,4,2]
impaire_paire :: [Int]->[Int]
impaire_paire x = if x==[] then [] else  [ x | x <- x, x `mod` 2 == 1]++(reverse[ x | x <- x, x `mod` 2 == 0])


-- Ex 7(5pts)- Le cryptage d'un message texte est le fait de le transformer de son format lisible et comprehensible a un format incomprehensible.

-- Une facon simple de faire un cryptage est de transformer les caracteres du message en code ascii puis changer la valeur de chaque code ascii 
-- en utilisant une fonction de cryptage d�finie, puis remettre le message crypt� en format texte (qui est devenu incomprehensible).

-- La pseudo-fonction a utilser pour cet exercice est la suivante : (crypter x = caractere ((code_ascii x) + y)) ou y est la cl� du cryptage utilis�e. 
-- Etant donn�e une cl� = 2, le cryptage du caractere "a" va etre: caractere (code_ascii "a" + 2) = caractere (97 + 2) = "c". 

-- Le decryptage ce fait en applicant la fonction inverse sur le message crypt�, 
-- en utilisant la meme cl� de cryptage. (decrypter x = caractere ((code_ascii x) - y)). caractere (code_ascii "c" - 2) = caractere (99 - 2) = "a"
-- Plus de documentation sur le cryptage : http://fr.wikipedia.org/wiki/Chiffrement

-- Cr�ez l'application "crypter_decrypter" qui fait le cryptage et le decryptage des messages texto. L'application necessite :
	-- - le type "Transfert" qui est compos� d'une cl� de cryptage/d�cryptage de type num�rique et du message a transmettre
data Transfert = Transfert (Int, String)

	-- a) (2pts) une fonction recursive "crypt" qui prend un parametre de type "Transfert" et retourne le message crypt�
crypter :: Transfert -> String
crypter (Transfert (n,xx))
	| (xx==[])=[]
	| otherwise = [toEnum(fromEnum (head xx) + n)::Char]++crypter (Transfert (n,tail xx))

	-- b) (2pts) une fonction recursive "decrypter" qui prend un parametre de type "Transfert" et retourne le message d�crypt�
decrypter :: Transfert -> String
decrypter (Transfert (n,xx))
	| (xx==[])=[]
	| otherwise = [toEnum(fromEnum (head xx) - n)::Char]++decrypter (Transfert (n,tail xx))

	-- c) (1pt) L'application "crypter_decrypter" prend en parametre un couple de type (String, Transfert):
		-- 1er cas : si la partie "String" est "E" alors l'application "crypter_decrypter" considere que c'est un envoie de message 
			-- et par consequence lance le cryptage du message enregistr� dans la partie "Transfert" en utilisant la cl� correspondante.
		-- 2eme cas : si la partie "String" est "R" alors l'application "crypter_decrypter" considere que c'est une reception de message 
			-- et par consequence lance le decryptage du message enregistr� dans la partie "Transfert" en utilisant la cl� correspondante.
		-- 3 eme cas : autrement l'application affiche "Operation non autorisee".
crypter_decrypter :: (String, Transfert) -> String
crypter_decrypter (msgtype, trans)
	| (msgtype=="E")= crypter trans
	| (msgtype=="R")= decrypter trans
	| otherwise = "Operation non autorisee"


-- Ex 8(6pts)- KNN (k-Nearest Neighbors) ou k plus proches voisins est un algorithme d'apprentissage supervis� 
-- utilis� principalement pour la classification dans l'intelligence artificielle. 

-- L'id�e g�n�rale de KNN est que �tant donn� un �chantillon de r�f�rence compos� de X instances dont on connait d�j� leur classe, 
-- pour une nouvelle instance Y(dont on connait pas sa classe) on associe la classe majoritaire des k instances de r�f�rence les plus proches de la nouvelle entr�e Y. 

-- Dans le cas o� K=1, on associe � Y la m�me classe de l'instance de r�f�rence la plus proche (la plus similaire). 
-- Plus de documentation sur k-Nearest Neighbors : http://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm

-- Cr�ez l'algorithme KNN pour K=1 (version 1NN)
	-- la description d'une instance est sous la forme d'une liste d'entiers (vecteurs) (exemple [1, 1, 2, 2, 1]). 
	-- Une instance est de type Knn_data et est repr�sent�e par le tuplet ([description], sa classe) (exemple x = ([1, 1, 2, 2, 1], "classe_de_x"))

type Knn_data = ([Integer], String)

	-- a) (2pts) Il faut d�finir une fonction recursive "euclidean_dist" qui permet de mesurer la similarit� (selon la distance euclidienne) entre deux vecteurs. 
	-- Cette fonction retourne la distance euclidienne entre les deux vecteurs
euclidean_dist::([Integer],[Integer])->Float
e_dist_somme::([Integer],[Integer])->Float
e_dist_somme ([],[]) = 0.0
e_dist_somme ((v1:vs1),(v2:vs2)) = (fromIntegral v2 - fromIntegral v1)^2 + e_dist_somme(vs1,vs2)
euclidean_dist ([],[]) = 0.0
euclidean_dist ((v1:vs1),(v2:vs2)) = sqrt(e_dist_somme((v1:vs1),(v2:vs2)))

	-- b) (2pts) �tant donn�e le vecteur repr�sentant une instance "y" dont on ne conait pas sa classe, la fonction recursive "dist_lst" prend en parametre ce vecteur (de y) 
	-- et la liste d'instances de r�f�rence (de type Knn_data), elle retourne la liste des distances entre "y" et toutes les instances de r�f�rence.
dist_lst::[Integer]->[Knn_data]->[Float]
dist_lst _ [] = []
dist_lst vec (l:ls) = [euclidean_dist(vec,fst l)] ++ dist_lst vec (ls)

	-- c) (2pts) la fonction "knn" repr�sente la fonction principale qui retourne la classe associ�e � l'instance "y" pass�e en param�tre (sous la forme de son vecteur de description). 
	-- La fonction recursive "knn" prend aussi en param�tre la liste d'instances de r�f�rence (de type Knn_data). 
	-- Cette fonction utilise les fonctions "dist_lst" et/ou "euclidean_dist" pour trouver la classe de l'instance "y"(il faut utiliser au moin une des deux fonctions).
knn::[Integer]->[Knn_data]->String
knn _ [] = ""
knn vect (k:ks) 
	| ((dist_lst vect [k]) == [minimum (dist_lst vect (k:ks))])==True = snd k
	| otherwise = knn vect ks

-- Fin






