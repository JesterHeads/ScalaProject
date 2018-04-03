package projet7

object jeuDeLaVie {
	type Grille=List[(Int,Int)]
			val testList=List("   "," X ","XXX") 
			val min=0
			val max=9
			//Question 1
			def chainesToGrille(l:List[String]):Grille={
		def chaineToGrilleAux(l:List[String],li:Int):Grille={
				l match{
				case Nil=>Nil
				case t::q=>stringConvert(t,li)++chaineToGrilleAux(q, li+1)
				}
		}
		chaineToGrilleAux(l, 0)
	}

	def stringConvert(s:String,li:Int):Grille={
		def stringConvertAux(s:String,i:Int):Grille={
				if(s.length>i){
					if(s.charAt(i)=='X'){
						(li,i)::stringConvertAux(s, i+1)
					}else stringConvertAux(s, i+1)
				} else Nil   
		}
		stringConvertAux(s, 0)
	}
	//Question 2

	val l = List("      ",
			"         ",
			"    X    ",
			"   XXX   ",
			"  XX XX  ",
			"   XXX   ",
			"    X    ",
			"         ",
			"         ") 
			val g:Grille=chainesToGrille(l) 

			def appartient(l:List[(Int,Int)],e:(Int,Int)):Boolean=l match{
			case Nil => false
			case x::q if(x==e)=> true
			case t::q=> appartient(q, e)
      }    

	def afficherGrille(g:Grille):Unit= {
			val lmin = (g foldLeft g.head._1)((x, y) => Math.min(y._1, x))
					val cmin = (g foldLeft g.head._2)((x, y) => Math.min(y._2, x))
					val lmax = (g foldLeft g.head._1)((x, y) => Math.max(y._1, x))
					val cmax = (g foldLeft g.head._2)((x, y) => Math.max(y._2, x))

					def dessine(l:Int,c:Int):Unit={
				if (g contains((l,c)))
					print('X')
					else
						print(" ")

						if (c<cmax)
							dessine(l,c+1)
							else
								if (l<lmax){
									println()
									dessine(l+1,cmin)
								}
			}
			dessine(lmin,cmin)
	}

	def voisines8(l:Int,c:Int):List[(Int,Int)]={
			val li:List[(Int,Int)]=List((l-1,c), (l+1,c), (l,c-1), (l,c+1), (l+1,c-1), (l+1,c+1),(l-1,c-1), (l-1,c+1))
					li
	}

	def voisinesVivante(g:Grille,l:List[(Int,Int)]):Int={
			l match{
			case Nil=>0
			case t::q=>{
				if(appartient(g, t)){
					1+voisinesVivante(g, q)
				}else{
					0+voisinesVivante(g, q)
				}
			 }
			}
	}

	def survivantes(g:Grille):Grille={
			g match{
			case Nil=>Nil
			case t::q =>{
				val z=voisinesVivante(g, voisines8(t._1, t._2))
						if(1<z && z<4){
							t::survivantes(q)
						}else{
							survivantes(q)
						}
			}
			}
	}

	def voisinesMortes(g:Grille,l:List[(Int,Int)]):Grille={
			l match{
			case Nil=>Nil
			case t::q=>{
				if(!appartient(g, t)){
					t::voisinesMortes(g, q)
				}else{
					voisinesMortes(g, q)
				}
			}
			}
	}

	def candidates(g:Grille):Grille={
			g match{
			case Nil=>Nil
			case t::q=>{
				val v=voisines8(t._1,t._2)
						voisinesMortes(g, v)++candidates(q)
			}
			}
	}

}